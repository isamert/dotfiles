#!/bin/bash

set -euo pipefail

case ${1:-""} in
    -f|--force)
        echo "=> Cleaning old files..."
        rm -f ~/.emacs.d/index.org
        rm -f ~/.emacs.d/index.el
        for file in ${PWD}/emacs/load/*.el; do
            [[ ! $file = *secrets* ]] && rm -f "${HOME}/.emacs.d/load/$(basename "${file}")"
        done
        ;;
esac

echo "=> Installing Emacs configuration..."
ln -s "${PWD}/emacs/index.org" ~/.emacs.d/index.org
ln -s "${PWD}/emacs/index.el" ~/.emacs.d/index.el

mkdir -p ~/.emacs/load/
for file in ${PWD}/emacs/load/*.el; do
    ln -s "${file}" "$HOME/.emacs.d/load/$(basename ${file})"
done

if [[ $EMACS_ONLY = 1 ]]; then
    exit 0;
fi

set +e
read -rd '' EMACS_EVAL <<EOF
(progn
  (require 'org)

  (add-hook
   'org-babel-post-tangle-hook
   '(lambda () (when (or (string-match-p "\\\\.\\\\(py\\\\|sh\\\\)$" (buffer-file-name))
                         (string-match-p "\\\\(python\\\\|sh\\\\)-mode" (symbol-name major-mode)))
                 (set-file-modes (buffer-file-name) #o755))))

  (defun when-darwin (file-path)
    (if (eq system-type 'darwin)
      (progn
        (message ":: Tangling %s" file-path)
        file-path)
      "no"))

  (defun when-linux (file-path)

    (if (eq system-type 'gnu/linux)
      (progn
        (message ":: Tangling %s" file-path)
        file-path)
      "no"))

  (cl-defun when-on (&key linux darwin)
    (pcase system-type
      ('darwin
        (message ":: Tangling %s" darwin)
        darwin)
      ('gnu/linux
        (message ":: Tangling %s" linux)
        linux)))

  (org-babel-tangle-file "./index.org"))
EOF
set -e

echo "=> Installing dotfiles..."
emacs -Q \
      --batch \
      --eval "$EMACS_EVAL" \
