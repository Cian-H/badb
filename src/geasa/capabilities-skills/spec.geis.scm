;;; test-skills.scm — Tests for Skill Loading (geasa capabilities-skills plugin)
;;;
;;; Verifies frontmatter parsing (well-formed, absent, unterminated),
;;; single-file loading, registry discovery across multiple roots with
;;; shadowing and unreadable entries, enable/disable filtering, reload,
;;; and prompt rendering.

(define-module (test test-skills)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (badb kernel registry)
  #:use-module (geasa capabilities-skills plugin))

(spec-begin "capabilities-skills")

;;;
;;; Fixture helpers
;;;

(define %root-a "/tmp/badb-skills-test-a")
(define %root-b "/tmp/badb-skills-test-b")

(define (write-text! path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (setup-fixtures!)
  (system (format #f "rm -rf ~a ~a" %root-a %root-b))
  ;; root A: a full skill with frontmatter, one without, and a bare dir.
  (system (format #f "mkdir -p ~a/alpha ~a/plain ~a/empty-dir" %root-a %root-a %root-a))
  (write-text! (string-append %root-a "/alpha/SKILL.md")
    "---\nname: Alpha Skill\ndescription: Drives the alpha workflow.\n---\n\n# Alpha\n\nRun the alpha steps in order.\n")
  (write-text! (string-append %root-a "/plain/SKILL.md")
    "# Just a body\n\nNo frontmatter here.\n")
  ;; root B: shadows alpha with different content, plus a broken skill.
  (system (format #f "mkdir -p ~a/alpha ~a/broken" %root-b %root-b))
  (write-text! (string-append %root-b "/alpha/SKILL.md")
    "---\nname: Shadowed Alpha\ndescription: Should lose to root A.\n---\n\nShadow body.\n")
  (write-text! (string-append %root-b "/broken/SKILL.md")
    "---\nname: Broken\nno closing delimiter ever\n"))

(define (teardown-fixtures!)
  (system (format #f "rm -rf ~a ~a" %root-a %root-b)))

;;;
;;; 1. Skill record construction
;;;

(test-group "skill-record"
  (let ((skill (make-skill "demo" #:name "Demo" #:description "A demo."
                           #:path "/tmp/x/SKILL.md" #:body "Body text.")))
    (test-assert "skill? recognizes record" (skill? skill))
    (test-equal "id preserved" "demo" (skill-id skill))
    (test-equal "name preserved" "Demo" (skill-name skill))
    (test-equal "description preserved" "A demo." (skill-description skill))
    (test-equal "path preserved" "/tmp/x/SKILL.md" (skill-path skill))
    (test-equal "body preserved" "Body text." (skill-body skill)))

  (let ((bare (make-skill "bare")))
    (test-equal "missing name falls back to id" "bare" (skill-name bare))
    (test-equal "missing description defaults to empty" "" (skill-description bare))
    (test-equal "missing body defaults to empty" "" (skill-body bare)))

  (test-assert "empty id rejected"
    (catch #t
      (lambda () (make-skill "") #f)
      (lambda args #t))))

;;;
;;; 2. Frontmatter parsing
;;;

(test-group "skills-frontmatter"
  (let* ((text "---\nname: Alpha Skill\ndescription: Drives the alpha workflow.\n---\n\n# Alpha\n\nRun the alpha steps.\n")
         (parsed (parse-skill-frontmatter text)))
    (test-equal "name parsed" "Alpha Skill" (assoc-ref parsed 'name))
    (test-equal "description parsed" "Drives the alpha workflow."
                (assoc-ref parsed 'description))
    (test-equal "body after closing delimiter"
      "# Alpha\n\nRun the alpha steps." (assoc-ref parsed 'body)))

  (let* ((text "# Just a body\n\nNo frontmatter here.\n")
         (parsed (parse-skill-frontmatter text)))
    (test-assert "no frontmatter: name is #f" (not (assoc-ref parsed 'name)))
    (test-assert "no frontmatter: description is #f"
      (not (assoc-ref parsed 'description)))
    (test-equal "no frontmatter: whole text is body"
      text (assoc-ref parsed 'body)))

  (let* ((text "---\nname: Broken\nno closing delimiter ever\n")
         (parsed (parse-skill-frontmatter text)))
    (test-assert "unterminated frontmatter: name is #f"
      (not (assoc-ref parsed 'name)))
    (test-equal "unterminated frontmatter: whole text is body"
      text (assoc-ref parsed 'body)))

  (let ((parsed (parse-skill-frontmatter "---")))
    (test-assert "bare delimiter alone: name is #f"
      (not (assoc-ref parsed 'name)))
    (test-equal "bare delimiter alone: body is the text"
      "---" (assoc-ref parsed 'body)))

  (let* ((text "---\nname: With: Colons\ndescription: value with : colon\n---\nbody\n")
         (parsed (parse-skill-frontmatter text)))
    (test-equal "value keeps colons after the first"
      "With: Colons" (assoc-ref parsed 'name))
    (test-equal "description keeps colons after the first"
      "value with : colon" (assoc-ref parsed 'description))))

;;;
;;; 3. File loading & registry discovery
;;;

(test-group "skills-discovery"
  (setup-fixtures!)
  (let* ((registry (make-skill-registry #:roots (list %root-a %root-b)))
         (_ (discover-skills! registry))
         (ids (skill-registry-ids registry)))
    (test-equal "discovers alpha, plain and broken across roots"
      '("alpha" "broken" "plain") (sort ids string<?))
    (test-equal "empty-dir (no SKILL.md) is skipped"
      #t (not (member "empty-dir" ids)))

    (let ((alpha (skill-registry-find registry "alpha")))
      (test-assert "alpha found" (and alpha (skill? alpha)))
      (test-equal "earlier root shadows later root on id collision"
        "Alpha Skill" (skill-name alpha))
      (test-equal "alpha path is from root A"
        (string-append %root-a "/alpha/SKILL.md") (skill-path alpha)))

    (let ((plain (skill-registry-find registry "plain")))
      (test-assert "plain skill found" (and plain (skill? plain)))
      (test-equal "skill without frontmatter keeps id as name"
        "plain" (skill-name plain))
      (test-assert "plain body contains its heading"
        (string-contains (skill-body plain) "# Just a body")))

    (let ((broken (skill-registry-find registry 'broken)))
      (test-assert "symbol id also resolves" (and broken (skill? broken)))
      (test-equal "unterminated frontmatter degrades to raw body"
        "---\nname: Broken\nno closing delimiter ever\n"
        (skill-body broken)))

    (test-assert "find on missing id returns #f"
      (not (skill-registry-find registry "nope")))

    ;; Reload after adding a new skill to root B.
    (system (format #f "mkdir -p ~a/late" %root-b))
    (write-text! (string-append %root-b "/late/SKILL.md")
      "---\nname: Late\n---\nLate body.\n")
    (skill-registry-reload! registry)
    (test-assert "reload picks up skills added after discovery"
      (skill-registry-find registry "late")))
  (teardown-fixtures!))

(test-group "skills-registry-validation"
  (test-assert "non-list roots rejected"
    (catch #t
      (lambda () (make-skill-registry #:roots "not-a-list") #f)
      (lambda args #t)))
  (test-assert "non-string root rejected"
    (catch #t
      (lambda () (make-skill-registry #:roots '(42)) #f)
      (lambda args #t)))
  (let ((registry (make-skill-registry #:roots '("/nonexistent-root-xyz"))))
    (discover-skills! registry)
    (test-equal "missing roots are skipped silently"
      '() (skill-registry-ids registry))))

;;;
;;; 4. Enable / disable filtering
;;;

(test-group "skills-enable-disable"
  (setup-fixtures!)
  (let* ((registry (make-skill-registry #:roots (list %root-a)))
         (_ (discover-skills! registry)))
    (test-equal "all skills active by default"
      '("alpha" "plain") (sort (map skill-id (skill-registry-active-skills registry))
                               string<?))
    (skill-registry-disable! registry "plain")
    (test-equal "disabled skill hidden from active list"
      '("alpha") (map skill-id (skill-registry-active-skills registry)))
    (test-equal "disabled skill still discoverable"
      "plain" (skill-id (skill-registry-find registry "plain")))
    (skill-registry-disable! registry "plain")
    (test-equal "double disable is idempotent"
      '("alpha") (map skill-id (skill-registry-active-skills registry)))
    (skill-registry-enable! registry "plain")
    (test-equal "re-enabled skill returns to active list"
      '("alpha" "plain") (sort (map skill-id (skill-registry-active-skills registry))
                               string<?))
    (test-assert "disable on unknown id errors"
      (catch #t
        (lambda () (skill-registry-disable! registry "ghost") #f)
        (lambda args #t)))
    (test-assert "enable on unknown id errors"
      (catch #t
        (lambda () (skill-registry-enable! registry "ghost") #f)
        (lambda args #t))))
  (teardown-fixtures!))

;;;
;;; 5. Prompt rendering
;;;

(test-group "skills-prompt"
  (setup-fixtures!)
  (let* ((registry (make-skill-registry #:roots (list %root-a)))
         (_ (discover-skills! registry))
         (alpha (skill-registry-find registry "alpha"))
         (prompt (skill-prompt alpha)))
    (test-assert "prompt opens with the skill tag and id"
      (string-prefix? "<skill id=\"alpha\" name=\"Alpha Skill\">" prompt))
    (test-assert "prompt carries the description"
      (string-contains prompt "Drives the alpha workflow."))
    (test-assert "prompt closes the tag"
      (string-contains prompt "</skill>"))
    (test-assert "prompt carries the body"
      (string-contains prompt "Run the alpha steps in order."))
    (test-assert "body follows the tag after a blank line"
      (string-contains prompt "</skill>\n\n# Alpha")))
  (teardown-fixtures!))

;;;
;;; 6. Catalog summary
;;;

(test-group "skills-catalog-summary"
  (setup-fixtures!)
  (let* ((registry (make-skill-registry #:roots (list %root-a)))
         (_ (discover-skills! registry))
         (summary (skill-catalog-summary registry)))
    (test-equal "summarizes active skills with id and description"
      '(("alpha" . "Drives the alpha workflow.")
        ("plain" . "# Just a body"))
      summary)

    ;; Disabled skills are excluded from summary
    (skill-registry-disable! registry "plain")
    (test-equal "disabled skill excluded from catalog summary"
      '(("alpha" . "Drives the alpha workflow."))
      (skill-catalog-summary registry))
    (skill-registry-enable! registry "plain")
    (test-equal "re-enabled skill appears in catalog summary"
      '(("alpha" . "Drives the alpha workflow.")
        ("plain" . "# Just a body"))
      (skill-catalog-summary registry))

    ;; Empty or whitespace-only description in frontmatter falls back to body
    (system (format #f "mkdir -p ~a/blank-desc" %root-a))
    (write-text! (string-append %root-a "/blank-desc/SKILL.md")
      "---\nname: Blank Desc\ndescription:   \n---\n\n\n  # First Real Line\n\nMore content.\n")
    (skill-registry-reload! registry)
    (let ((reloaded-summary (skill-catalog-summary registry)))
      (test-equal "empty frontmatter description falls back to first non-empty body line"
        "# First Real Line"
        (assoc-ref reloaded-summary "blank-desc")))

    (test-assert "skill-catalog-summary rejects non-skill-registry"
      (catch #t
        (lambda () (skill-catalog-summary "not-a-reg") #f)
        (lambda args #t))))
  (teardown-fixtures!))

;;;
;;; 7. Tool registration (skill_load, skill_list, _badb_defer flags)
;;;

(test-group "skills-tool-registration"
  (setup-fixtures!)
  (let* ((skill-reg (make-skill-registry #:roots (list %root-a)))
         (_ (discover-skills! skill-reg))
         (tool-reg (make-registry))
         (_ (register-skill-tools! tool-reg skill-reg)))

    ;; Tool attachment verification
    (test-assert "skill_load is attached to registry"
      (procedure? (registry-lookup tool-reg "skill_load")))
    (test-assert "skill_list is attached to registry"
      (procedure? (registry-lookup tool-reg "skill_list")))

    ;; _badb_defer flags verification
    (let ((load-meta (registry-metadata tool-reg "skill_load"))
          (list-meta (registry-metadata tool-reg "skill_list")))
      (test-assert "skill_load metadata present" (pair? load-meta))
      (test-assert "skill_list metadata present" (pair? list-meta))

      (test-equal "skill_load assoc-ref _badb_defer is #f"
        #f
        (assoc-ref load-meta '_badb_defer))

      (test-equal "skill_list assoc-ref _badb_defer is #t"
        #t
        (assoc-ref list-meta '_badb_defer))

      ;; Parameters schema validation
      (let* ((load-params (assoc-ref load-meta 'parameters))
             (load-req (assoc-ref load-params 'required))
             (list-params (assoc-ref list-meta 'parameters))
             (list-req (assoc-ref list-params 'required)))
        (test-assert "skill_load required is a vector" (vector? load-req))
        (test-equal "skill_load required contains id" #("id") load-req)
        (test-assert "skill_list required is a vector" (vector? list-req))
        (test-equal "skill_list required is empty vector" #() list-req)))

    ;; skill_load with valid ID returns skill prompt
    (let ((out (registry-dispatch tool-reg "skill_load" "alpha")))
      (test-assert "skill_load returns string for valid id" (string? out))
      (test-assert "skill_load output begins with skill tag"
        (string-prefix? "<skill id=\"alpha\" name=\"Alpha Skill\">" out))
      (test-assert "skill_load output contains prompt body"
        (string-contains out "Run the alpha steps in order."))
      (test-equal "skill_load output matches skill-prompt"
        (skill-prompt (skill-registry-find skill-reg "alpha"))
        out))

    ;; skill_load with unknown ID returns informative error listing available skill IDs
    (let ((err-out (registry-dispatch tool-reg "skill_load" "ghost-skill")))
      (test-assert "skill_load returns string for unknown id" (string? err-out))
      (test-assert "error message mentions Error or not found"
        (and (string-contains err-out "Error")
             (string-contains err-out "ghost-skill")))
      (test-assert "error message lists available skill IDs (alpha)"
        (string-contains err-out "alpha"))
      (test-assert "error message lists available skill IDs (plain)"
        (string-contains err-out "plain")))

    ;; skill_list returns formatted list of discovered skill IDs, status, and paths
    (let ((list-out (registry-dispatch tool-reg "skill_list")))
      (test-assert "skill_list returns string" (string? list-out))
      (test-assert "skill_list mentions alpha" (string-contains list-out "alpha"))
      (test-assert "skill_list mentions plain" (string-contains list-out "plain"))
      (test-assert "skill_list indicates active status for alpha"
        (string-contains list-out "active"))
      (test-assert "skill_list contains file path"
        (string-contains list-out (string-append %root-a "/alpha/SKILL.md"))))

    ;; skill_list reflects disabled status
    (skill-registry-disable! skill-reg "plain")
    (let ((list-out-disabled (registry-dispatch tool-reg "skill_list")))
      (test-assert "skill_list reflects disabled status for plain"
        (string-contains list-out-disabled "disabled")))

    ;; Validation of arguments to register-skill-tools!
    (test-assert "register-skill-tools! rejects invalid tool-reg"
      (catch #t
        (lambda () (register-skill-tools! "not-a-reg" skill-reg) #f)
        (lambda args #t)))
    (test-assert "register-skill-tools! rejects invalid skill-reg"
      (catch #t
        (lambda () (register-skill-tools! tool-reg "not-a-reg") #f)
        (lambda args #t))))
  (teardown-fixtures!))

(spec-end)
(spec-exit)
