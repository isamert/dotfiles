#!/bin/bash

set -euo pipefail

case ${1:-""} in
-h | --help)
    echo "./install.sh [--force] [--help]"
    exit 0
    ;;
-f | --force)
    echo "=> Cleaning old files..."
    rm -f ~/.emacs.d/index.org
    rm -f ~/.emacs.d/index.el
    for file in "${PWD}/emacs/load"/*.el; do
        [[ ! $file = *secrets* ]] && rm -f "${HOME}/.emacs.d/load/$(basename "${file}")"
    done
    ;;
esac

echo "=> Installing Emacs configuration..."
ln -s "${PWD}/emacs/index.org" ~/.emacs.d/index.org
ln -s "${PWD}/emacs/index.el" ~/.emacs.d/index.el

mkdir -p ~/.emacs.d/load/
for file in "${PWD}/emacs/load"/*.el; do
    ln -s "${file}" "$HOME/.emacs.d/load/$(basename "${file}")"
done

set +e
read -rd '' EMACS_EVAL <<EOF
(progn
  (require 'org)

  (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (python . t)
       (js . t)))

  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-post-tangle-hook 'executable-make-buffer-file-executable-if-script-p)

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

cd emacs

echo "=> Tangling Emacs config..."
emacs -Q \
    --batch \
    --eval "$EMACS_EVAL"

cd ..

echo "=> Installing dotfiles..."
emacs -Q \
    --batch \
    --eval '(setq org-babel-noweb-wrap-start "<<<")' \
    --eval '(setq org-babel-noweb-wrap-end ">>>")' \
    --eval "$EMACS_EVAL"
