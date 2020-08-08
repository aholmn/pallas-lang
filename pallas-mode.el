(setq pallas-highlights
      '(("def\\|do\\|end\\|var\\|return\\|if\\|else\\|return" . font-lock-keyword-face)
        ("\\(\\_<[a-zA-Z_][a-zA-Z0-9_]*\\_>\\)[(]" 1 font-lock-function-name-face)
        ("true\\|false" . font-lock-constant-face)))

(define-derived-mode pallas-mode fundamental-mode "pallas"
  "major mode for editing pallas-lang code."
  (setq font-lock-defaults '(pallas-highlights)))
