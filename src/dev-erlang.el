(use-package erlang
  :config
  (progn
    (require 'erlang-start)
    (add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
    (add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))))

(use-package erlstack-mode)
