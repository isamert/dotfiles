;;; im-ef-solstice-theme.el --- Light theme blending Ef Summer + Acme paper -*- lexical-binding:t -*-

;; Copyright (C) 2026
;; Author: Generated
;; Keywords: faces, theme, accessibility

;;; Commentary:
;; A light, paper-like theme that mixes the warm Plan 9/Acme base with
;; Ef Summer’s playful magenta/purple accents and crisp blues/cyans.

;;; Code:

(require 'ef-themes)

(defconst im-ef-solstice-palette-partial
  '((cursor "#b0007a")
    (bg-main "#fff9e8")     ; acme-like paper
    (bg-dim "#f2edd9")
    (bg-alt "#e8e2cf")
    (fg-main "#4a3f55")     ; slightly purple-leaning ink (summer vibe)
    (fg-dim "#7b6f63")
    (fg-alt "#8a3f73")
    (bg-active "#d6cfbb")
    (bg-inactive "#fffdf2")
    (border "#c2b9a6")

    ;; Core chroma: Acme warmth + Ef Summer accents
    (red "#b3192a")
    (red-warmer "#d1002e")
    (red-cooler "#a01b55")
    (red-faint "#b04d58")

    (green "#1f6b3a")
    (green-warmer "#3a7400")
    (green-cooler "#0a6f63")
    (green-faint "#5e7467")

    (yellow "#8b5a25")
    (yellow-warmer "#a04f2c")
    (yellow-cooler "#965052")
    (yellow-faint "#8a6b6b")

    (blue "#2f5bd6")
    (blue-warmer "#4a54e8")
    (blue-cooler "#0a63ff")
    (blue-faint "#5a66c8")

    (magenta "#a13aa2")
    (magenta-warmer "#b31a96")
    (magenta-cooler "#7b49e6")
    (magenta-faint "#9a5b90")

    (cyan "#1f6ab3")
    (cyan-warmer "#3b6aa6")
    (cyan-cooler "#0f7689")
    (cyan-faint "#5b66b0")

    ;; Intense backgrounds (still “paper-friendly”)
    (bg-red-intense "#ffb7bf")
    (bg-green-intense "#a7e7b0")
    (bg-yellow-intense "#ffd66a")
    (bg-blue-intense "#d3dcff")
    (bg-magenta-intense "#f0b9ff")
    (bg-cyan-intense "#a8dbff")

    ;; Subtle backgrounds
    (bg-red-subtle "#ffdbe0")
    (bg-green-subtle "#d4f3dc")
    (bg-yellow-subtle "#f6ecc6")
    (bg-blue-subtle "#dde6ff")
    (bg-magenta-subtle "#f7dcf6")
    (bg-cyan-subtle "#d3f0f2")

    ;; Diffs
    (bg-added "#d4f3dc")
    (bg-added-faint "#eaf8ee")
    (bg-added-refine "#c6edd2")
    (fg-added "#0f4a1a")

    (bg-changed "#ffe6bf")
    (bg-changed-faint "#fff1d5")
    (bg-changed-refine "#ffd49f")
    (fg-changed "#5a3e00")

    (bg-removed "#ffd6e2")
    (bg-removed-faint "#ffe4eb")
    (bg-removed-refine "#f6bfd0")
    (fg-removed "#8a1016")

    ;; UI accents
    (bg-mode-line-active "#f0c2e9")  ; summer-ish, but softened for paper bg
    (fg-mode-line-active "#2f2442")

    (bg-completion "#e2e8ff")
    (bg-hover "#bfe0ff")
    (bg-hover-secondary "#cfeedd")
    (bg-hl-line "#ffe0e8")
    (bg-paren-match "#b7d2f2")
    (bg-err "#ffd6ea")
    (bg-warning "#ffedc8")
    (bg-info "#cfeedd")
    (bg-region "#f0dbb0")))

(defconst im-ef-solstice-palette-mappings-partial
  '((err red-warmer)
    (warning yellow)
    (info green-cooler)

    (fg-link blue)
    (fg-link-visited magenta)
    (name magenta-warmer)
    (keybind blue-warmer)
    (identifier magenta-faint)
    (fg-prompt magenta-warmer)

    (builtin magenta)
    (comment yellow-faint)          ; summer-like warm comments
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

(defcustom im-ef-solstice-palette-overrides nil
  "Overrides for `im-ef-solstice-palette'.

Mirror the elements of the palette, overriding their value.

For overrides shared across all Ef themes, refer to
`ef-themes-common-palette-overrides'."
  :group 'ef-themes
  :package-version '(ef-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

(defconst im-ef-solstice-palette
  (modus-themes-generate-palette
   im-ef-solstice-palette-partial
   nil
   nil
   (append im-ef-solstice-palette-mappings-partial ef-themes-palette-common)))

(modus-themes-theme
 'im-ef-solstice
 'ef-themes
 "Legible light paper theme: Acme warmth with Ef Summer magenta/cyan sparkle."
 'light
 'im-ef-solstice-palette
 nil
 'im-ef-solstice-palette-overrides
 'ef-themes-custom-faces)

(provide 'im-ef-solstice-theme)
;;; im-ef-solstice-theme.el ends here
