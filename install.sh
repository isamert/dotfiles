#!/bin/bash

set -euo pipefail

case $1 in
    -f|--force)
        echo "=> Cleaning old files..."
        rm ~/.emacs.d/index.org
        for file in ${PWD}/emacs/load/*.el; do
            rm "${HOME}/.emacs.d/load/$(basename "${file}")"
        done
        ;;
esac

echo "=> Installing Emacs configuration..."
ln -s "${PWD}/emacs/index.org" ~/.emacs.d/index.org

mkdir -p ~/.emacs/load/
for file in ${PWD}/emacs/load/*.el; do
    ln -s "${file}" "$HOME/.emacs.d/load/$(basename ${file})"
done

# This hook is required to make script files exacutable while they are
# being exported
read -r -d '' EMACS_INSTALL_POST_TANGLE_HOOK <<EOF
(add-hook
 'org-babel-post-tangle-hook
 '(lambda () (when (or (string-match-p "\\\\.\\\\(py\\\\|sh\\\\)$" (buffer-file-name))
                       (string-match-p "\\\\(python\\\\|sh\\\\)-mode" (symbol-name major-mode)))
               (set-file-modes (buffer-file-name) #o755))))

(defun when-darwin (file-path)
  (when (eq system-type 'darwin)
    file-path))

(defun when-linux (file-path)
  (when (eq system-type 'gnu/linux)
    file-path))

(cl-defun when-on (&key linux darwin)
  (case system-type
    ('darwin darwin)
    ('gnu/linux linux)))
EOF

echo "=> Installing dotfiles..."
emacs -Q \
      --batch \
      --eval "(require 'org)" \
      --eval "$EMACS_INSTALL_POST_TANGLE_HOOK" \
      --eval '(org-babel-tangle-file "./index.org")'
