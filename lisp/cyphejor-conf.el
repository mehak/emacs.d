;;; -*- lexical-binding: t; -*-

(setq
 cyphejor-rules
 '(:upcase
   ("bookmark" "→")
   ("buffer" "β")
   ("diff" "Δ")
   ("dired" "δ")
   ("emacs" "ε")
   ("erc" "erc")
   ("eshell" "esh")
   ("exwm" "Θ")
   ("inferior" "i" :prefix)
   ("interaction" "i" :prefix)
   ("interactive" "i" :prefix)
   ("lisp" "λ" :postfix)
   ("menu" "▤" :postfix)
   ("mode" "")
   ("org" "org")
   ("package" "↓")
   ("python" "π")
   ("shell" "sh" :postfix)
   ("text" "ξ")
   ("wdired" "↯δ")))

(cyphejor-mode 1)
