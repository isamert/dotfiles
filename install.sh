#!/bin/bash

set -euo pipefail

FRESH=0
EMACS_PROFILE="${HOME}/.emacs.d"
EMACS_PACKAGES_DIR="${EMACS_PROFILE}/packages"
EMACS_LOAD_DIR="${EMACS_PROFILE}/load"

case ${1:-""} in
-h | --help)
    echo "./install.sh [--fresh] [--help]"
    echo
    echo "  --fresh will install everything from scratch."
    echo "  To simply update, run without arguments."
    exit 0
    ;;
--fresh)
    FRESH=1
    ;;
esac

shopt -s nullglob

stow --target=$HOME/.emacs.d/ .emacs.d

if [[ $FRESH != 1 ]]; then
    echo "If you want to install from scratch, run this script with --fresh parameter."
    exit 0
fi

if ! which emacs; then
    echo ""
    read -r -p ">> Emacs is not installed, want to install it? [y/n]" choice
    if [[ $choice =~ ^[Yy]$ ]]; then
        case $(uname) in
        Darwin*)
            brew tap d12frosted/emacs-plus
            brew install emacs-plus@29 \
                --with-dragon-icon \
                --with-dbus \
                --with-no-frame-refocus \
                --with-native-comp \
                --with-imagemagick \
                --with-poll \
                --with-xwidgets
            ;;
        Linux*)
            sudo pacman -S emacs
            ;;
        esac
    else
        echo "Exiting..."
        exit 1
    fi
fi

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

echo "=> Installing dotfiles..."
emacs -Q \
    --batch \
    --eval '(setq org-babel-noweb-wrap-start "<<<")' \
    --eval '(setq org-babel-noweb-wrap-end ">>>")' \
    --eval "$EMACS_EVAL"
