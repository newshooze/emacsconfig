
(deftheme pasta "Pasta")

(let (
(color0 "#202020")
(color1 "#c5c8c6")
(color2 "#384247")
(color3 "#8abeb7")
(color4 "#515b60")
(color5 "#3d474c")
(color6 "#30383c")
(color7 "#d9dcda")
(color8 "#444c50")
(color9 "#edf0ee")
(color10 "#20292d")
(color11 "#1c2428")
(color12 "#6f797e")
(color13 "#81a2be")
(color14 "#6c7a80")
(color15 "#de935f")
(color16 "#b294bb")
(color17 "#b5bd68")
(color18 "#a6e22e")
(color19 "#616462")
(color20 "#333d42")
(color21 "#d4d7d5")
(color22 "#2f393e")
(color23 "#d0d3d1"))


(custom-theme-set-faces
'pasta

`(default ((t (:background ,color0 :foreground ,color1 ))))
`(hl-line ((t (:background ,color2 ))))
`(cursor ((t (:foreground ,color3 ))))
`(region ((t (:background ,color4 ))))
`(secondary-selection ((t (:background ,color5 ))))
`(fringe ((t (:background ,color0 ))))
`(mode-line-inactive ((t (:background ,color6 :foreground ,color7 ))))
`(mode-line ((t (:background ,color8 :foreground ,color9 ))))
`(minibuffer-prompt ((t (:background ,color10 :foreground ,color1 ))))
`(border ((t (:background ,color11 :foreground ,color11 ))))
`(vertical-border ((t (:foreground ,color12 ))))

`(font-lock-builtin-face ((t (:foreground ,color13 ))))
`(font-lock-comment-face ((t (:foreground ,color14 :fontStyle :italic t ))))
`(font-lock-constant-face ((t (:foreground ,color15 ))))
`(font-lock-function-name-face ((t (:foreground ,color13 ))))
`(font-lock-keyword-face ((t (:foreground ,color16 ))))
`(font-lock-string-face ((t (:foreground ,color17 ))))
`(font-lock-type-face ((t (:foreground ,color18 ))))))

(custom-theme-set-variables
  'pasta
  '(linum-format " %3i "))

(provide-theme 'pasta)
