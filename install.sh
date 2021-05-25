#!/bin/bash

set -euo pipefail

case $1 in
    -f|--force)
        echo "=> Cleaning old files..."
        rm ~/.emacs.d/README.org
        for file in ${PWD}/emacs/load/*.el; do
            rm "${HOME}/.emacs.d/load/$(basename "${file}")"
        done
        ;;
esac

echo "=> Installing Emacs configuration..."
ln -s "${PWD}/emacs/README.org" ~/.emacs.d/README.org

mkdir -p ~/.emacs/load/
for file in ${PWD}/emacs/load/*.el; do
    ln -s "${file}" "$HOME/.emacs.d/load/$(basename ${file})"
done

# This hook is required to make script files exacutable while they are
# being exported
read -r -d '' EMACS_INSTALL_POST_TANGLE_HOOK <<EOF
(add-hook
 'org-babel-post-tangle-hook
 '(lambda () (when (or (string-match-p "\\\\(python\\\\|sh\\\\)" (buffer-file-name))
                       (string-match-p "\\\\(python\\\\|sh\\\\)"  (symbol-name major-mode)))
               (set-file-modes (buffer-file-name) #o755))))
EOF

echo "=> Installing dotfiles..."
emacs -Q \
      --batch \
      --eval "(require 'org)" \
      --eval "$EMACS_INSTALL_POST_TANGLE_HOOK" \
      --eval '(org-babel-tangle-file "./README.org")'
