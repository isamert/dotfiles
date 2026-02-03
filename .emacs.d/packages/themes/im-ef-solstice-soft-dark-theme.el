;;; im-ef-solstice-soft-dark-theme.el --- Soft dark variant of im-ef-solstice -*- lexical-binding:t -*-

;; Copyright (C) 2026
;; Author: Generated
;; Keywords: faces, theme, accessibility

;;; Commentary:
;; A soft, low-contrast dark theme with a warm “ink on dusk paper” feel,
;; keeping the Solstice magenta/cyan accents without harsh blacks.

;;; Code:

(require 'ef-themes)

(defconst im-ef-solstice-soft-dark-palette-partial
  '((cursor "#ff5bc4")
    (bg-main "#1f1a21")     ; warm, slightly purple-black (not harsh)
    (bg-dim "#241f26")
    (bg-alt "#2b2430")
    (fg-main "#e7d9e6")     ; soft off-white with a hint of lilac
    (fg-dim "#bfaebd")
    (fg-alt "#d7a6c8")
    (bg-active "#3a313f")
    (bg-inactive "#1b161e")
    (border "#463b4a")

    ;; Core chroma: softened, slightly desaturated for dark bg
    (red "#ff6a7a")
    (red-warmer "#ff4f6a")
    (red-cooler "#ff5aa8")
    (red-faint "#d58a93")

    (green "#68d08a")
    (green-warmer "#86d65a")
    (green-cooler "#4fd1c0")
    (green-faint "#99b9a3")

    (yellow "#e2b36b")
    (yellow-warmer "#f0a66b")
    (yellow-cooler "#e6a0a8")
    (yellow-faint "#c9aaad")

    (blue "#86a8ff")
    (blue-warmer "#9b95ff")
    (blue-cooler "#76b7ff")
    (blue-faint "#a2a9d6")

    (magenta "#f08cff")
    (magenta-warmer "#ff74d6")
    (magenta-cooler "#b89cff")
    (magenta-faint "#d3a4cf")

    (cyan "#7bd3ff")
    (cyan-warmer "#8fd0e6")
    (cyan-cooler "#55d1c8")
    (cyan-faint "#9bb2d7")

    ;; Intense backgrounds (used for search etc) — keep soft
    (bg-red-intense "#4a2630")
    (bg-green-intense "#203a2b")
    (bg-yellow-intense "#4a3a21")
    (bg-blue-intense "#252f4f")
    (bg-magenta-intense "#3a2146")
    (bg-cyan-intense "#1f3a3f")

    ;; Subtle backgrounds
    (bg-red-subtle "#352128")
    (bg-green-subtle "#1f2f26")
    (bg-yellow-subtle "#33291e")
    (bg-blue-subtle "#21273a")
    (bg-magenta-subtle "#2e1f33")
    (bg-cyan-subtle "#1d2d30")

    ;; Diffs (soft and readable)
    (bg-added "#1f3328")
    (bg-added-faint "#1b2a22")
    (bg-added-refine "#254032")
    (fg-added "#86e6a5")

    (bg-changed "#3a2f1f")
    (bg-changed-faint "#31281b")
    (bg-changed-refine "#4a3b24")
    (fg-changed "#ffd28a")

    (bg-removed "#3a2028")
    (bg-removed-faint "#311b21")
    (bg-removed-refine "#4a2630")
    (fg-removed "#ff9aa5")

    ;; UI accents
    (bg-mode-line-active "#3b2a3d")  ; plum, understated
    (fg-mode-line-active "#f1e3ef")

    (bg-completion "#272f46")
    (bg-hover "#2b3a4a")
    (bg-hover-secondary "#22362c")
    (bg-hl-line "#2a222d")
    (bg-paren-match "#2f3a52")
    (bg-err "#3a202b")
    (bg-warning "#3a2f22")
    (bg-info "#21352a")
    (bg-region "#3a3142")))

(defconst im-ef-solstice-soft-dark-palette-mappings-partial
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

(defcustom im-ef-solstice-soft-dark-palette-overrides nil
  "Overrides for `im-ef-solstice-soft-dark-palette'.

Mirror the elements of the palette, overriding their value.

For overrides shared across all Ef themes, refer to
`ef-themes-common-palette-overrides'."
  :group 'ef-themes
  :package-version '(ef-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

(defconst im-ef-solstice-soft-dark-palette
  (modus-themes-generate-palette
   im-ef-solstice-soft-dark-palette-partial
   nil
   nil
   (append im-ef-solstice-soft-dark-palette-mappings-partial ef-themes-palette-common)))

(modus-themes-theme
 'im-ef-solstice-soft-dark
 'ef-themes
 "Soft dark variant of im-ef-solstice: warm dusk background with gentle magenta/cyan accents."
 'dark
 'im-ef-solstice-soft-dark-palette
 nil
 'im-ef-solstice-soft-dark-palette-overrides
 'ef-themes-custom-faces)

(provide 'im-ef-solstice-soft-dark-theme)
;;; im-ef-solstice-soft-dark-theme.el ends here
