;;; im-ef-solstice-dark-theme.el --- Dark variant of im-ef-solstice -*- lexical-binding:t -*-

;; Copyright (C) 2026
;; Author: Generated
;; Keywords: faces, theme, accessibility

;;; Commentary:
;; A dark companion to `im-ef-solstice': muted “night paper” background
;; with Ef Summer magenta/cyan sparkle and Acme-like warmth.

;;; Code:

(require 'ef-themes)

(defconst im-ef-solstice-dark-palette-partial
  '((cursor "#ff4fc4")
    (bg-main "#1a1614")
    (bg-dim "#211b18")
    (bg-alt "#2a221e")
    (fg-main "#e9d8e6")
    (fg-dim "#b9a79f")
    (fg-alt "#f09acb")
    (bg-active "#3a2f2a")
    (bg-inactive "#171312")
    (border "#4a3c35")

    (red "#ff5a6a")
    (red-warmer "#ff3c57")
    (red-cooler "#ff4aa0")
    (red-faint "#d7868e")

    (green "#52d18a")
    (green-warmer "#7ad24a")
    (green-cooler "#34d1bf")
    (green-faint "#93b8aa")

    (yellow "#f1b06a")
    (yellow-warmer "#ff9a66")
    (yellow-cooler "#f29aa0")
    (yellow-faint "#c6a7a3")

    (blue "#7aa2ff")
    (blue-warmer "#9a90ff")
    (blue-cooler "#63b3ff")
    (blue-faint "#a3a9e6")

    (magenta "#ff78e8")
    (magenta-warmer "#ff4fd6")
    (magenta-cooler "#b38cff")
    (magenta-faint "#d2a0c8")

    (cyan "#66bfff")
    (cyan-warmer "#8ab8ff")
    (cyan-cooler "#4ed0de")
    (cyan-faint "#a0b2d8")

    ;; Intense backgrounds (dark)
    (bg-red-intense "#4a1e23")
    (bg-green-intense "#1f3a2a")
    (bg-yellow-intense "#4a3520")
    (bg-blue-intense "#232a4a")
    (bg-magenta-intense "#3e2346")
    (bg-cyan-intense "#1b3a3f")

    ;; Subtle backgrounds
    (bg-red-subtle "#2f1a1c")
    (bg-green-subtle "#18261e")
    (bg-yellow-subtle "#2b221a")
    (bg-blue-subtle "#1a1f2f")
    (bg-magenta-subtle "#261a2a")
    (bg-cyan-subtle "#17262a")

    ;; Diffs
    (bg-added "#1c3425")
    (bg-added-faint "#17261d")
    (bg-added-refine "#244b33")
    (fg-added "#7fe7a7")

    (bg-changed "#3a2f1d")
    (bg-changed-faint "#2a2217")
    (bg-changed-refine "#544027")
    (fg-changed "#ffd08a")

    (bg-removed "#3a1d25")
    (bg-removed-faint "#2a171c")
    (bg-removed-refine "#542733")
    (fg-removed "#ff9aa6")

    ;; UI accents
    (bg-mode-line-active "#3b2236")
    (fg-mode-line-active "#f3e6ff")

    (bg-completion "#1a2435")
    (bg-hover "#243c55")
    (bg-hover-secondary "#1f3a2a")
    (bg-hl-line "#2a1d22")
    (bg-paren-match "#2a3a5a")
    (bg-err "#3a1826")
    (bg-warning "#3a2a18")
    (bg-info "#183026")
    (bg-region "#3a2c20")))

(defconst im-ef-solstice-dark-palette-mappings-partial
  '((err red-warmer)
    (warning yellow)
    (info green-cooler)

    (fg-link blue-cooler)
    (fg-link-visited magenta)
    (name magenta-warmer)
    (keybind blue-warmer)
    (identifier magenta-faint)
    (fg-prompt magenta-warmer)

    (builtin magenta)
    (comment yellow-faint)
    (constant red-cooler)
    (fnname magenta-warmer)
    (fnname-call magenta-faint)
    (keyword magenta-cooler)
    (preprocessor green-cooler)
    (docstring cyan-faint)
    (string yellow-warmer)
    (type cyan-warmer)
    (variable blue-warmer)
    (variable-use blue-faint)
    (rx-backslash cyan-cooler)
    (rx-construct red-cooler)

    (accent-0 magenta-cooler)
    (accent-1 yellow)
    (accent-2 cyan-cooler)
    (accent-3 red)

    (date-common cyan-cooler)
    (date-deadline red-warmer)
    (date-deadline-subtle red-faint)
    (date-event fg-alt)
    (date-holiday magenta-warmer)
    (date-now fg-main)
    (date-range fg-alt)
    (date-scheduled yellow)
    (date-scheduled-subtle yellow-faint)
    (date-weekday cyan)
    (date-weekend red-faint)

    (fg-prose-code yellow)
    (prose-done green-cooler)
    (fg-prose-macro cyan-cooler)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table fg-alt)
    (prose-table-formula info)
    (prose-tag yellow-faint)
    (prose-todo red-warmer)
    (fg-prose-verbatim magenta-cooler)

    (mail-cite-0 yellow-cooler)
    (mail-cite-1 magenta)
    (mail-cite-2 blue-warmer)
    (mail-cite-3 cyan-warmer)
    (mail-part magenta-faint)
    (mail-recipient magenta-warmer)
    (mail-subject magenta-cooler)
    (mail-other magenta)

    (bg-search-static bg-warning)
    (bg-search-current bg-yellow-intense)
    (bg-search-lazy bg-blue-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (rainbow-0 magenta-warmer)
    (rainbow-1 magenta-cooler)
    (rainbow-2 yellow)
    (rainbow-3 cyan)
    (rainbow-4 magenta)
    (rainbow-5 blue-warmer)
    (rainbow-6 red-cooler)
    (rainbow-7 cyan-cooler)
    (rainbow-8 yellow-cooler)))

(defcustom im-ef-solstice-dark-palette-overrides nil
  "Overrides for `im-ef-solstice-dark-palette'."
  :group 'ef-themes
  :package-version '(ef-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

(defconst im-ef-solstice-dark-palette
  (modus-themes-generate-palette
   im-ef-solstice-dark-palette-partial
   nil
   nil
   (append im-ef-solstice-dark-palette-mappings-partial ef-themes-palette-common)))

(modus-themes-theme
 'im-ef-solstice-dark
 'ef-themes
 "Dark companion to im-ef-solstice: warm night-paper with magenta/cyan accents."
 'dark
 'im-ef-solstice-dark-palette
 nil
 'im-ef-solstice-dark-palette-overrides
 'ef-themes-custom-faces)

(provide 'im-ef-solstice-dark-theme)
;;; im-ef-solstice-dark-theme.el ends here
