
-- {{{ Main
theme = {}
local home = os.getenv("HOME")
local theme_dir = home .. "/.config/awesome/theme"

theme.wallpaper = home .. "/.wallpaper/current.jpg"
-- }}}

-- {{{ Styles
theme.font      = "sans 8"

-- {{{ Colors
theme.fg_normal  = "#DCDCCC"
theme.fg_focus   = "#F0DFAF"
theme.fg_urgent  = "#CC9393"
theme.bg_normal  = "#050608"
theme.bg_focus   = "#050608"
theme.bg_urgent  = "#050608"
theme.bg_systray = theme.bg_normal

-- Change if you want to color focus in tasklist
theme.tasklist_fg_focus = "#B1917A"
-- }}}

-- {{{ Borders
theme.border_width  = 2
theme.border_normal = "#3F3F3F"
theme.border_focus  = "#6F6F6F"
theme.border_marked = "#CC9393"
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = "#3F3F3F"
theme.titlebar_bg_normal = "#3F3F3F"
-- }}}

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- Example:
--theme.taglist_bg_focus = "#CC9393"
-- }}}

-- {{{ Widgets
-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
theme.bg_widget        = "#333333"
theme.fg_widget        = "#908884"
theme.fg_center_widget = "#636363"
theme.fg_end_widget    = "#ffffff"
theme.fg_off_widget    = "#22211f"
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = "#CC9393"
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = 15
theme.menu_width  = 125
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = theme_dir .. "/taglist/squarefz.png"
theme.taglist_squares_unsel = theme_dir .. "/taglist/squarez.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc
theme.awesome_icon      = theme_dir .. "/icons/awesome-icon.png"
theme.menu_submenu_icon = theme_dir .. "/icons/submenu.png"
theme.arch_icon         = theme_dir .. "/icons/arch.png"
theme.widget_net        = theme_dir .. "/icons/net_down_02.png"
theme.widget_netup      = theme_dir .. "/icons/net_up_02.png"
theme.mpd_play          = theme_dir .. "/icons/note.png"
theme.mpd_stop          = theme_dir .. "icons/stop.png"
theme.mpd_pause         = theme_dir .. "/icons/pause.png"
theme.mute              = theme_dir .. "/icons/mute.png"
theme.vol               = theme_dir .. "/icons/vol.png"
theme.bat               = theme_dir .. "/icons/bat.png"
theme.ac                = theme_dir .. "/icons/ac.png"
theme.batfull           = theme_dir .. "/icons/batfull.png"
theme.batlow            = theme_dir .. "/icons/batlow.png"
theme.batmed            = theme_dir .. "/icons/batmed.png"
theme.disc              = theme_dir .. "/icons/disc.png"
theme.half              = theme_dir .. "/icons/half.png"
theme.full              = theme_dir .. "/icons/full.png"
theme.wifi              = theme_dir .. "/icons/wifi.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = theme_dir .. "/layouts/tile.png"
theme.layout_tileleft   = theme_dir .. "/layouts/tileleft.png"
theme.layout_tilebottom = theme_dir .. "/layouts/tilebottom.png"
theme.layout_tiletop    = theme_dir .. "/layouts/tiletop.png"
theme.layout_fairv      = theme_dir .. "/layouts/fairv.png"
theme.layout_fairh      = theme_dir .. "/layouts/fairh.png"
theme.layout_spiral     = theme_dir .. "/layouts/spiral.png"
theme.layout_dwindle    = theme_dir .. "/layouts/dwindle.png"
theme.layout_max        = theme_dir .. "/layouts/max.png"
theme.layout_fullscreen = theme_dir .. "/layouts/fullscreen.png"
theme.layout_magnifier  = theme_dir .. "/layouts/magnifier.png"
theme.layout_floating   = theme_dir .. "/layouts/floating.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus  = theme_dir .. "/titlebar/close_focus.png"
theme.titlebar_close_button_normal = theme_dir .. "/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active  = theme_dir .. "/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active = theme_dir .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = theme_dir .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = theme_dir .. "/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active  = theme_dir .. "/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active = theme_dir .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = theme_dir .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = theme_dir .. "/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active  = theme_dir .. "/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active = theme_dir .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = theme_dir .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = theme_dir .. "/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active  = theme_dir .. "/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active = theme_dir .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme_dir .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme_dir .. "/titlebar/maximized_normal_inactive.png"
-- }}}
-- }}}

return theme
