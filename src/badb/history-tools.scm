;;; badb — Conversation History Summarization Facade
;;;
;;; Backwards compatibility facade for (badb history-tools), re-exporting all
;;; public symbols from unprivileged capability subsystem in (badb capabilities history-tools).

(define-module (badb history-tools)
  #:use-module (badb capabilities history-tools)
  #:re-export (jsonl-line->turn
               summarize-turns
               summarize-history-file
               turn-field
               user-turn?
               extract-words
               meaningful-word?
               word-frequencies
               top-topic-words
               %stopwords
               %summary-recent-user-count
               %summary-recent-user-chars
               %summary-topic-count))
