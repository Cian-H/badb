;;; badb — Skill Loading
;;;
;;; Discovers and loads agent skills laid out as <root>/<skill-id>/SKILL.md
;;; (the Agent Skills format: optional YAML-style frontmatter delimited by
;;; '---' lines carrying name and description, followed by markdown body).
;;; A skill registry scans a list of search roots (BADB_SKILL_PATH, or
;;; ".agents/skills" by default), builds immutable <skill> records, and
;;; renders them as prompt blocks for injection into a session.

(define-module (geasa capabilities-skills plugin)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (badb kernel registry)
  #:export (<skill>
            make-skill
            skill?
            skill-id
            skill-name
            skill-description
            skill-path
            skill-body
            <skill-registry>
            make-skill-registry
            skill-registry?
            skill-registry-roots
            skill-registry-skills
            skill-search-path
            parse-skill-frontmatter
            load-skill-file
            discover-skills!
            skill-registry-ids
            skill-registry-find
            skill-registry-list
            skill-registry-active-skills
            skill-registry-enable!
            skill-registry-disable!
            skill-registry-reload!
            skill-prompt
            skill-catalog-summary
            register-skill-tools!))

;;;
;;; Skill record
;;;

(define-record-type <skill>
  (%make-skill id name description path body)
  skill?
  (id skill-id)
  (name skill-name)
  (description skill-description)
  (path skill-path)
  (body skill-body))

(define* (make-skill id #:key (name #f) (description #f) (path #f) (body ""))
  "Build a <skill>.  ID is the skill identifier (directory basename);
NAME and DESCRIPTION come from frontmatter when present."
  (unless (and (string? id) (not (string-null? id)))
    (error "make-skill: id must be a non-empty string"))
  (%make-skill id
               (or (and (string? name) name) id)
               (if (string? description) description "")
               path
               (if (string? body) body "")))

;;;
;;; Frontmatter parsing
;;;

(define (string-prefix-line? prefix s)
  "True when S begins with PREFIX followed by end-of-line."
  (and (>= (string-length s) (string-length prefix))
       (string= prefix s 0 (string-length prefix) 0 (string-length prefix))
       (or (= (string-length s) (string-length prefix))
           (char=? #\newline (string-ref s (string-length prefix))))))

(define* (parse-skill-frontmatter text)
  "Parse SKILL.md content TEXT.  Returns an alist
((name . VALUE) (description . VALUE) (body . STRING)); when TEXT has no
well-formed frontmatter block, name and description are #f and the whole
TEXT is the body."
  (define (frontmatter-line? line)
    (or (string=? line "---") (string=? line "--- ")))
  (define (parse-key-value line)
    (let ((idx (string-index line #\:)))
      (and idx
           (let ((key (string-trim-both (substring line 0 idx)))
                 (value (string-trim-both (substring line (+ idx 1)))))
             (and (not (string-null? key))
                  (cons key value))))))
  (if (string-prefix-line? "---" text)
      (let* ((rest (if (< (string-length text) 4) "" (substring text 4)))
             (lines (string-split rest #\newline)))
        (let loop ((pending lines)
                   (fields '())
                   (closed? #f)
                   (body-lines '()))
          (cond
           ((and (null? pending) (not closed?))
            ;; No closing delimiter: treat the entire text as body.
            `((name . #f) (description . #f) (body . ,text)))
           ((null? pending)
            (let* ((rev-lines (reverse body-lines))
                   (trimmed-top (if (and (pair? rev-lines) (string-null? (car rev-lines)))
                                    (cdr rev-lines)
                                    rev-lines))
                   (clean-body (if (and (pair? trimmed-top) (string-null? (last trimmed-top)))
                                   (drop-right trimmed-top 1)
                                   trimmed-top)))
              `((name . ,(assoc-ref fields "name"))
                (description . ,(assoc-ref fields "description"))
                (body . ,(string-join clean-body "\n")))))
           ((and (not closed?) (frontmatter-line? (car pending)))
            (loop (cdr pending) fields #t body-lines))
           ((not closed?)
            (let ((kv (parse-key-value (car pending))))
              (loop (cdr pending)
                    (if kv (cons kv fields) fields)
                    closed?
                    body-lines)))
           (else
            (loop (cdr pending) fields closed?
                  (cons (car pending) body-lines))))))
      `((name . #f) (description . #f) (body . ,text))))

;;;
;;; File loading
;;;

(define (read-file-softly path)
  "Contents of PATH as a string, or #f when unreadable."
  (catch #t
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (set-port-encoding! port "UTF-8")
          (get-string-all port))))
    (lambda _ #f)))

(define* (load-skill-file path id)
  "Load a single SKILL.md at PATH under identifier ID.  Returns a <skill>
or #f when the file is unreadable."
  (let ((text (read-file-softly path)))
    (and text
         (let* ((parsed (parse-skill-frontmatter text))
                (name (assoc-ref parsed 'name))
                (description (assoc-ref parsed 'description))
                (body (assoc-ref parsed 'body)))
           (make-skill id
                       #:name name
                       #:description description
                       #:path path
                       #:body body)))))

;;;
;;; Registry
;;;

(define-record-type <skill-registry>
  (%make-skill-registry roots skills disabled)
  skill-registry?
  (roots skill-registry-roots)
  (skills skill-registry-skills set-skill-registry-skills!)
  (disabled skill-registry-disabled set-skill-registry-disabled!))

(define (skill-search-path)
  "Skill search roots: BADB_SKILL_PATH split on colons, or
'(\".agents/skills\") when unset or empty."
  (let ((path (getenv "BADB_SKILL_PATH")))
    (if (or (not path) (string-null? path))
        '(".agents/skills")
        (filter (lambda (s) (not (string-null? s)))
                (string-split path #\:)))))

(define* (make-skill-registry #:key (roots (skill-search-path)))
  "Build an empty <skill-registry> over ROOTS (list of directory paths)."
  (unless (and (list? roots) (every string? roots))
    (error "make-skill-registry: roots must be a list of strings"))
  (%make-skill-registry roots '() '()))

(define (directory-entries dir)
  "Sorted subdirectory names of DIR, or '() when DIR is unreadable."
  (catch #t
    (lambda ()
      (let* ((raw (scandir dir (lambda (n) (not (member n '("." ".."))))))
             (entries (filter (lambda (n)
                                (let ((full (string-append dir "/" n)))
                                  (catch #t
                                    (lambda ()
                                      (eq? (stat:type (stat full)) 'directory))
                                    (lambda _ #f))))
                              (or raw '()))))
        (sort entries string<?)))
    (lambda _ '())))

(define (discover-skills! registry)
  "Scan REGISTRY's roots for <skill-id>/SKILL.md and load each into the
registry.  Earlier roots shadow later ones on id collision; directories
without a readable SKILL.md are skipped.  Returns REGISTRY."
  (let ((found '()))
    (for-each
     (lambda (root)
       (for-each
        (lambda (entry)
          (unless (assoc entry found)
            (let* ((path (string-append root "/" entry "/SKILL.md"))
                   (skill (load-skill-file path entry)))
              (when skill (set! found (acons entry skill found))))))
        (directory-entries root)))
     (skill-registry-roots registry))
    (set-skill-registry-skills! registry (reverse found))
    registry))

(define (skill-registry-ids registry)
  "Ids of all discovered skills, discovery order."
  (map car (skill-registry-skills registry)))

(define (skill-registry-find registry id)
  "The <skill> for ID (string or symbol), or #f."
  (let ((sid (if (string? id) id (format #f "~a" id))))
    (and=> (assoc sid (skill-registry-skills registry)) cdr)))

(define (skill-registry-list registry)
  "All discovered <skill> records."
  (map cdr (skill-registry-skills registry)))

(define (skill-registry-active-skills registry)
  "Discovered <skill> records that are not disabled."
  (let ((disabled (skill-registry-disabled registry)))
    (filter (lambda (skill)
              (not (member (skill-id skill) disabled)))
            (skill-registry-list registry))))

(define (skill-registry-disable! registry id)
  "Mark skill ID disabled; hidden from ACTIVE-SKILLS until re-enabled."
  (let ((skill (skill-registry-find registry id)))
    (unless skill
      (error (format #f "skill-registry-disable!: no such skill: ~a" id)))
    (let ((sid (skill-id skill))
          (disabled (skill-registry-disabled registry)))
      (unless (member sid disabled)
        (set-skill-registry-disabled! registry (cons sid disabled))))
    registry))

(define (skill-registry-enable! registry id)
  "Re-enable a disabled skill ID."
  (let ((skill (skill-registry-find registry id)))
    (unless skill
      (error (format #f "skill-registry-enable!: no such skill: ~a" id)))
    (set-skill-registry-disabled!
     registry (remove (lambda (sid) (string=? sid (skill-id skill)))
                     (skill-registry-disabled registry)))
    registry))

(define (skill-registry-reload! registry)
  "Re-run discovery, clearing the skill table (disabled ids are kept)."
  (set-skill-registry-skills! registry '())
  (discover-skills! registry))

;;;
;;; Prompt rendering
;;;

(define (skill-prompt skill)
  "Render SKILL as a prompt block for injection into a session:
a delimited header carrying id, name and description, then the body."
  (format #f "<skill id=\"~a\" name=\"~a\">~%~a~%</skill>~%~%~a"
          (skill-id skill)
          (skill-name skill)
          (skill-description skill)
          (skill-body skill)))

;;;
;;; Catalog Summary
;;;

(define (extract-first-non-empty-line text)
  "Extract the first non-empty line from TEXT, trimmed of leading/trailing whitespace."
  (let ((lines (string-split (or text "") #\newline)))
    (let loop ((rest lines))
      (cond
       ((null? rest) "")
       (else
        (let ((line (string-trim-both (car rest))))
          (if (string-null? line)
              (loop (cdr rest))
              line)))))))

(define (skill-effective-description skill)
  "Return SKILL's frontmatter description if present and non-empty,
otherwise the first non-empty line of its body."
  (let ((desc (skill-description skill)))
    (if (and (string? desc) (not (string-null? (string-trim-both desc))))
        desc
        (extract-first-non-empty-line (skill-body skill)))))

(define (skill-catalog-summary skill-reg)
  "Return an alist ((id . description) ...) for all active skills in SKILL-REG.
If a skill's frontmatter description is missing or empty, extract the first
non-empty line of its body."
  (unless (skill-registry? skill-reg)
    (error "skill-catalog-summary: expected <skill-registry>" skill-reg))
  (map (lambda (skill)
         (cons (skill-id skill) (skill-effective-description skill)))
       (skill-registry-active-skills skill-reg)))

;;;
;;; Tool Registration
;;;

(define (register-skill-tools! tool-reg skill-reg)
  "Attach skill_load and skill_list tools into kernel TOOL-REG backed by SKILL-REG."
  (unless (registry? tool-reg)
    (error "register-skill-tools!: expected <registry>" tool-reg))
  (unless (skill-registry? skill-reg)
    (error "register-skill-tools!: expected <skill-registry>" skill-reg))

  (registry-attach!
   tool-reg
   "skill_load"
   (lambda* (id #:rest _rest)
     (let* ((sid (cond ((string? id) id)
                       ((symbol? id) (symbol->string id))
                       ((not id) "")
                       (else (format #f "~a" id))))
            (skill (and (not (string-null? sid))
                        (skill-registry-find skill-reg sid))))
       (if skill
           (skill-prompt skill)
           (let ((ids (skill-registry-ids skill-reg)))
             (format #f "Error: skill '~a' not found. Available skills: ~a"
                     sid
                     (if (null? ids)
                         "none"
                         (string-join ids ", ")))))))
   '((description . "Load a skill's full prompt instructions into context by its ID.")
     (parameters .
      ((type . "object")
       (properties .
        ((id . ((type . "string")
                (description . "Identifier of the skill to load")))))
       (required . #("id"))))
     (_badb_defer . #f)))

  (registry-attach!
   tool-reg
   "skill_list"
   (lambda* (#:rest _rest)
     (let ((skills (skill-registry-list skill-reg)))
       (if (null? skills)
           "No skills discovered."
           (string-join
            (map (lambda (skill)
                   (let* ((sid (skill-id skill))
                          (disabled? (member sid (skill-registry-disabled skill-reg)))
                          (status (if disabled? "disabled" "active"))
                          (path (or (skill-path skill) "")))
                     (format #f "- ~a [~a]: ~a" sid status path)))
                 skills)
            "\n"))))
   '((description . "List all discovered skills with their status and file paths.")
     (parameters .
      ((type . "object")
       (properties . ())
       (required . #())))
     (_badb_defer . #t)))

  tool-reg)
