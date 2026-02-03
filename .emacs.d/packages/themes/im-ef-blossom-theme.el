;;; im-ef-blossom-theme.el --- Legible light theme with soft pink and warm earth tones -*- lexical-binding:t -*-

;; Copyright (C) 2026 Isa Mert Gurbuz

;; Author: Generated
;; Maintainer: Generated
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A light theme blending ef-summer's warm pink palette with ef-acme's
;; muted paper-like quality. Soft blossom pink backgrounds with earthy
;; syntax colors for a calm, spring-like coding experience.

;;; Code:

(require 'ef-themes)

(defconst im-ef-blossom-palette-partial
  '((cursor "#9a4080")
    (bg-main "#FFF8F5")
    (bg-dim "#F5EAE6")
    (bg-alt "#EBDDD8")
    (fg-main "#4a4045")
    (fg-dim "#8a7a7e")
    (fg-alt "#885577")
    (bg-active "#DCCDC8")
    (bg-inactive "#FCF4F2")
    (border "#C8B8B5")

    ;; Core colors - blending summer's vibrancy with acme's muted tones
    (red "#a03030")
    (red-warmer "#b82a45")
    (red-cooler "#9a3065")
    (red-faint "#a05555")
    (green "#2a6840")
    (green-warmer "#4a7030")
    (green-cooler "#1a7565")
    (green-faint "#5a7560")
    (yellow "#8a6528")
    (yellow-warmer "#9a5535")
    (yellow-cooler "#8a5a48")
    (yellow-faint "#8a7058")
    (blue "#4060a8")
    (blue-warmer "#5050b8")
    (blue-cooler "#2868b0")
    (blue-faint "#6068a0")
    (magenta "#905088")
    (magenta-warmer "#a04090")
    (magenta-cooler "#7050a8")
    (magenta-faint "#906878")
    (cyan "#307888")
    (cyan-warmer "#406878")
    (cyan-cooler "#1a7080")
    (cyan-faint "#507080")

    ;; Intense backgrounds
    (bg-red-intense "#F8D8D8")
    (bg-green-intense "#D8F0D8")
    (bg-yellow-intense "#F0E8B8")
    (bg-blue-intense "#D8E8F8")
    (bg-magenta-intense "#F0D8F0")
    (bg-cyan-intense "#D0F0F0")

    ;; Subtle backgrounds
    (bg-red-subtle "#FCE8E5")
    (bg-green-subtle "#E5F5E8")
    (bg-yellow-subtle "#F8F0E0")
    (bg-blue-subtle "#E8F0FC")
    (bg-magenta-subtle "#F8E8F5")
    (bg-cyan-subtle "#E5F5F5")

    ;; Diff backgrounds
    (bg-added "#E5F5E8")
    (bg-added-faint "#F0FAF2")
    (bg-added-refine "#D5EAD8")
    (fg-added "#2a5838")

    (bg-changed "#F8F0E0")
    (bg-changed-faint "#FCF5E8")
    (bg-changed-refine "#F0E5C8")
    (fg-changed "#6a5020")

    (bg-removed "#FCE8E5")
    (bg-removed-faint "#FDF0EE")
    (bg-removed-refine "#F5D8D5")
    (fg-removed "#8a2020")

    ;; Mode line - soft rose tone
    (bg-mode-line-active "#F0D8E0")
    (fg-mode-line-active "#4a3848")

    ;; Special backgrounds
    (bg-completion "#F5E0E8")
    (bg-hover "#D8E8F0")
    (bg-hover-secondary "#E0F0E5")
    (bg-hl-line "#F5EAE8")
    (bg-paren-match "#D0E8E8")
    (bg-err "#FCE0E0")
    (bg-warning "#F8F0D8")
    (bg-info "#E0F5E8")
    (bg-region "#E8D8E8")))

(defconst im-ef-blossom-palette-mappings-partial
  '((err red-warmer)
    (warning yellow)
    (info green)

    (fg-link blue-cooler)
    (fg-link-visited magenta-cooler)
    (name magenta)
    (keybind magenta-warmer)
    (identifier magenta-faint)
    (fg-prompt magenta)

    (builtin magenta-cooler)
    (comment yellow-faint)
    (constant red-cooler)
    (fnname magenta)
    (fnname-call magenta-faint)
    (keyword blue-warmer)
    (preprocessor green-cooler)
    (docstring cyan-faint)
    (string yellow-warmer)
    (type cyan-warmer)
    (variable blue)
    (variable-use blue-faint)
    (rx-backslash cyan-cooler)
    (rx-construct red-cooler)

    (accent-0 magenta)
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
    (prose-done green)
    (fg-prose-macro cyan-cooler)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table fg-alt)
    (prose-table-formula info)
    (prose-tag yellow-faint)
    (prose-todo red-warmer)
    (fg-prose-verbatim magenta)

    (mail-cite-0 yellow-cooler)
    (mail-cite-1 magenta)
    (mail-cite-2 blue)
    (mail-cite-3 cyan)
    (mail-part magenta-faint)
    (mail-recipient magenta)
    (mail-subject blue-warmer)
    (mail-other cyan)

    (bg-search-static bg-warning)
    (bg-search-current bg-yellow-intense)
    (bg-search-lazy bg-blue-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (rainbow-0 magenta)
    (rainbow-1 blue-warmer)
    (rainbow-2 yellow)
    (rainbow-3 cyan)
    (rainbow-4 red-cooler)
    (rainbow-5 green)
    (rainbow-6 magenta-cooler)
    (rainbow-7 yellow-cooler)
    (rainbow-8 blue)))

(defcustom im-ef-blossom-palette-overrides nil
  "Overrides for `im-ef-blossom-palette'."
  :group 'ef-themes
  :package-version '(ef-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

(defconst im-ef-blossom-palette
  (modus-themes-generate-palette
   im-ef-blossom-palette-partial
   nil
   nil
   (append im-ef-blossom-palette-mappings-partial ef-themes-palette-common)))

(modus-themes-theme
 'im-ef-blossom
 'ef-themes
 "Legible light theme with soft pink and warm earth tones."
 'light
 'im-ef-blossom-palette
 nil
 'im-ef-blossom-palette-overrides
 'ef-themes-custom-faces)

(provide 'im-ef-blossom-theme)
;;; im-ef-blossom-theme.el ends here
