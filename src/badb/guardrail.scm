;;; badb — Pluggable Guardrails & Policy Engine
;;;
;;; Backwards compatibility facade for (badb guardrail), re-exporting all
;;; public symbols from unprivileged policy subsystem in (badb policies guardrail).

(define-module (badb guardrail)
  #:use-module (badb policies guardrail)
  #:re-export (<guardrail-policy>
               make-guardrail-policy
               guardrail-policy?
               guardrail-policy-level
               guardrail-policy-rules
               guardrail-policy-evaluator
               guardrail-policy-journal
               set-guardrail-policy-level!
               set-guardrail-policy-rules!
               set-guardrail-policy-evaluator!
               set-guardrail-policy-journal!
               ;; Pre-built evaluators & rule constructors
               make-pattern-rule
               make-scheme-predicate-evaluator
               make-system1-model-evaluator
               default-cautious-evaluator
               dangerous-operation?
               mutating-operation?
               ;; Decision evaluation & level inspection
               evaluate-guardrail
               guardrail-level-rank
               guardrail-allow?
               guardrail-deny?
               guardrail-ask?))
