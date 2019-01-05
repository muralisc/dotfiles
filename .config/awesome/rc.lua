-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local vicious = require("vicious")
local hotkeys_popup = require("awful.hotkeys_popup").widget
awful.util.spawn_with_shell("compton --inactive-dim 0.2 -b")
awful.util.spawn_with_shell("urxvtd -q -o -f &")
awful.util.spawn_with_shell("xdg_menu --format awesome --root-menu /etc/xdg/menus/arch-applications.menu >~/.config/awesome/archmenu.lua")
awful.util.spawn_with_shell("xrdb ~/.Xresources")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("~/.config/awesome/theme.lua")
require("volume")
require("music")
require("brightness")
local xdg_menu = require("archmenu")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "vi"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
altkey = "Mod1"

mouseLeft = 1
mouseMiddle = 2
mouseRight = 3
mouseRollDown =4
mouseRollUp = 5

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,             --RESIZE dont work use mod+shift+h/l
    -- awful.layout.suit.fair.horizontal,  --RESIZE dont work
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}



-- {{{ Desktop Menu
-- Create a laucher widget and a main menu
local xdg_menu = require("archmenu")
myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal },
                                    { "ranger", "urxvtc -fn 'xft:UbuntuMono:Regular:size=25' -e ranger " },
                                    { "screen off", "xset s 10 10" },
                                    { "screen on", "xset s 7200 7200" },
                                    { "clock", "urxvtc -fn 'xft:UbuntuMono:Regular:size=25' -bg rgba:0000/0000/0000/cccc -e ncmpcpp -s clock" },
                                    { "applications", xdgmenu },
                                    { "morc_menu", "morc_menu" },
                                    { "shutdown", 'poweroff' },
                                  },
                          theme = { font="Ubuntu 18"; width = 290; height = 30; }
                        })


mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclockTimeout = 1
mytextclock = awful.widget.textclock(" %a, %Y %b %d, %H:%M:%S ", mytextclockTimeout )
mytextclock:buttons(awful.util.table.join(
     awful.button({ }, 1, function() awful.util.spawn_with_shell("urxvtc -hold -e cal -y") end)
            ))
-- }}}
--{{{ CPU widget
cpuwidgetTimeout = 2
cpuwidget = awful.widget.graph()
cpuwidget:set_width(50)
cpuwidget:set_background_color("#494B4F")
cpuwidget:set_border_color("#000000")
cpuwidget:set_color({ type = "linear",
                        from = { 0, 0 },
                        to = { 10,0 },
                        stops = { {0, "#FF5656"},
                            {0.5, "#88A175"},
                            {1, "#AECF96" }
                        }
                    })
cpuwidget_t = awful.tooltip({ objects = { cpuwidget },theme = { font="Ubuntu Mono bold 14"; width = 800; height=30 } })
-- Register widget
vicious.register(cpuwidget, vicious.widgets.cpu,
                    function (widget, args)
                        if( args[1] > 99 ) then
                            naughty.notify({
                                  title    = "HIGH CPU"
                                , text     = "Burning at " ..args[1]
                                , timeout  = 5
                                , position = "bottom_right"
                                , fg       = beautiful.fg_focus
                                , bg       = beautiful.bg_focus
                                -- , height   = 50
                                -- , width    = 1000
                                , font     = "Ubuntu Mono 20"
                            })
                        end
                        cpuwidget_t:set_text(""
                        .. string.format("%3d%% Cpu 1\n",args[2])
                        .. string.format("%3d%% Cpu 2\n", (args[3] and args[3] or 0) )
                        .. string.format("%3d%% Cpu 3\n", (args[4] and args[4] or 0) )
                        .. string.format("%3d%% Cpu 4",   (args[5] and args[5] or 0) )
                        )
                        return args[1]
                    end,
                    cpuwidgetTimeout
                )
--}}}
-- {{{ hdd temp widget
tempWidget = wibox.widget.textbox()
hddwidgetTimeout = 37
vicious.register(tempWidget,
                 vicious.widgets.thermal,
                 " <span color='#FFFF00'>$1°С </span>",
                 hddwidgetTimeout ,
                 "thermal_zone0")
-- }}}
--{{{ Memory widget
-- Initialize widget
memwidget = wibox.widget.textbox()
memwidgetTimeout = 13
-- Progressbar properties
memwidget_t = awful.tooltip({ objects = { memwidget },})
-- Register widget
vicious.register(       memwidget
                    ,   vicious.widgets.mem
                    ,   function (widget, args)
                            memwidget_t:set_text(
                            string.format(" %4d %-5s \n" ,args[3],"total"    )..
                            string.format(" %4d %-5s \n" ,args[2],"Used"     )..
                            string.format(" %4d %-5s \n" ,args[4],"free"     )..
                            string.format(" %4d %-5s \n" ,args[6],"swap%"    )..
                            string.format(" %4d %-5s"    ,args[7],"swap "    )
                            )
                            return args[1].."%"
                        end
                    ,   memwidgetTimeout)
--}}}
--the link obtained below may not be wifi
active_interface='lo'
-- {{{ wifi widget
wifiwidget = wibox.widget.textbox()
wifiwidgetTimeout = 5
wifiwidget_timer = timer({timeout = wifiwidgetTimeout})
wifiwidget_timer:connect_signal("timeout",
    function()
        active_interface=io.popen("ip route get 8.8.8.8 |awk '{print $5}'"):read()
        if active_interface == nil then
            active_interface='lo'
        end
        SSID=io.popen("iw dev " .. active_interface .. " link |awk -F: '/SSID/ {print $2}'"):read()
        signal = io.popen("iw dev " .. active_interface .. " link |awk -F: '/signal/ {print $2}'"):read()
        wifiwidget:set_markup(
            string.format("<span color='#5882FA'>| %s %s %s </span>",
            active_interface,
            SSID and SSID or "", -- lua terinary operator( var and true or false)
            signal and signal or ""
            )
        )
    end
    )
wifiwidget_timer:start()
-- }}}
-- {{{ network graph widget
netgraphTimeout = 2
netgraph = awful.widget.graph()
netgraph:set_width(50)
netgraph:set_border_color("#000000")
netgraph:set_background_color("#494B4F")
netgraph:set_color("#FF5656")
netgraph:set_scale(true)   -- auto scale
netgraph_t = awful.tooltip({ objects = { netgraph },})
vicious.cache(vicious.widgets.net)
vicious.register(netgraph, vicious.widgets.net,
                    function (widget, args)
                        netgraph_t:set_text("".. active_interface .. "\n" ..
                        string.format("%2.3f GB Recieved\n" ,args["{"..active_interface.." rx_gb}"])..
                        string.format("%2.3f GB Txn-ed\n"   ,args["{"..active_interface.." tx_gb}"])..
                        string.format("%2.3f Mbps down\n"   ,args["{"..active_interface.." down_mb}"])..
                        string.format("%2.3f Mbps up"       ,args["{"..active_interface.." up_mb}"])..
                        ""
                        )
                        return args["{"..active_interface.." down_kb}"]
                    end,
                    netgraphTimeout
                    )
-- }}}
-- {{{ battery widget
local battery = require("battery")
batterywidget = wibox.widget.textbox()
batterywidgetTimeout = 10
batterywidget_timer = timer({timeout = batterywidgetTimeout})
batterywidget_timer:connect_signal("timeout", function()
    batterywidget:set_markup(batteryInfo("BAT0").."|")
end)
batterywidget_timer:start()

-- }}}

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 2, function (c)
                                              c:kill()
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[2])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mylayoutbox,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            wibox.widget.systray(),
            cpuwidget,
            memwidget,
            netgraph,
            tempWidget,
            wifiwidget,
            batterywidget,
            mytextclock,
            brightness_widget,
            volume_widget,
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    -- control + semicolon is used by keynav
    -- mappping inspiraton from https://github.com/tpope/tpope
    awful.key({ modkey,           }, "bracketleft", function () update_volume(volume_widget, "~/bin/vol-control softer" ) end),
    awful.key({ modkey,           }, "]",           function () update_volume(volume_widget, "~/bin/vol-control louder" ) end),
    awful.key({ modkey,"Shift"    }, "[",           function () update_music( "mpc seek -5" ) end),
    awful.key({ modkey,"Shift"    }, "bracketright",function () update_music( "mpc seek +5" ) end),
    awful.key({ modkey,"Control"  }, "[",           function () update_volume(volume_widget, "~/bin/vol-control 0" ) end),
    awful.key({ modkey,"Control"  }, "]",           function () update_volume(volume_widget, "~/bin/vol-control 100" ) end),
    awful.key({ modkey, altkey    }, "bracketleft", function () update_music( "mpc prev" ) end),
    awful.key({ modkey, altkey    }, "bracketright",function () update_music( "mpc next" ) end),
    awful.key({ modkey,           }, "backslash",   function () awful.util.spawn("mpc toggle") end),
    awful.key({ modkey, "Control" }, "backslash" ,  function () awful.util.spawn("urxvtc -fn 'xft:UbuntuMono:Regular:size=25' -bg rgba:0000/0000/0000/1111 -e ncmpcpp -s clock ") end),
    awful.key({ modkey, altkey    }, "backslash",   function () awful.util.spawn("pavucontrol") end),
    awful.key({ modkey, altkey,
                "Control","Shift" }, "space" ,      function () awful.util.spawn("urxvtc -e poweroff") end),
    awful.key({ modkey ,          }, "=" ,          function () update_b(brightness_widget , "light -A 10" ) end),
    awful.key({ modkey ,          }, "-" ,          function () update_b(brightness_widget , "light -U 10" ) end),
    awful.key({modkey, "Control"  }, "=" ,          function () update_b(brightness_widget , "light -S 100" ) end),
    awful.key({modkey, "Control"  },  "-" ,         function () update_b(brightness_widget , "light -S 10" ) end),
    awful.key({                   }, "Print" ,      function () awful.util.spawn_with_shell('import $HOME/selection-`date +%Y-%m-%d_%H-%M-%S`.png') end), -- Print Screen : take screenshot
    awful.key({ "Shift"           }, "Print",       function () awful.util.spawn_with_shell('import -window root $HOME/selection-`date +%Y-%m-%d_%H-%M-%S`.png') end),
    awful.key({                   }, "Scroll_Lock", function () awful.util.spawn_with_shell("exec ~/.config/awesome/lockScript.sh") end), -- Lock screen
    awful.key({                   }, "Pause" ,      function () awful.util.spawn("urxvtc -e ranger") end),      -- launch fileexplorer
    awful.key({ altkey,           }, "Tab" ,        function () awful.util.spawn("rofi -show combi -font 'Ubuntu mono 22'") end),-- window switcher
    awful.key({ altkey, "Control" }, "s",           function () awful.util.spawn_with_shell( "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')") end),
    awful.key({ modkey            }, "q",           function () awful.util.spawn_with_shell("exec ~/.config/awesome/lockScript.sh show") end), -- show a quote
    -- window moving and resizing  {{{

    awful.key({ modkey            }, ".",           function () awful.client.moveresize(  0,   0, 40,  0) end),   -- Super >       -increase window width
    awful.key({ modkey            }, ",",           function () awful.client.moveresize(  0,   0,-40,  0) end),   -- Super <       -decrease window width
    awful.key({ modkey, "Shift"   }, ".",           function () awful.client.moveresize(  0,   0,  0, 40) end),   -- Super Shift > increase window height
    awful.key({ modkey, "Shift"   }, ",",           function () awful.client.moveresize(  0,   0,  0,-40) end),   -- Super Shift < decrease window height
    awful.key({ modkey            }, "Down",        function () awful.client.moveresize(  0,  20,  0,  0) end),   -- Super DownArrow: move window
    awful.key({ modkey            }, "Up",          function () awful.client.moveresize(  0, -20,  0,  0) end),   -- Super UpArrow
    awful.key({ modkey,           }, "Left",        function () awful.client.moveresize(-20,   0,  0,  0) end),
    awful.key({ modkey,           }, "Right",       function () awful.client.moveresize( 20,   0,  0,  0) end),

    -- }}}
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return",
              function ()
                  local matcher = function (c) return awful.rules.match(c, {class = 'URxvt'}) end
                  awful.client.run_or_raise( terminal , matcher)
              end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Shift"   }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey, altkey    }, "h",     awful.tag.viewprev,
              {description = "View preivous tag", group = "layout"}),
    awful.key({ modkey, altkey    }, "l",     awful.tag.viewnext,
              {description = "View next tag", group = "layout"}),
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey, altkey    }, "j",     function () awful.client.incwfact( 0.05) end,
              {description = "increase client width|height", group = "layout"}),
    awful.key({ modkey, altkey    }, "k",     function () awful.client.incwfact( -0.05) end,
              {description = "decrease client width|height", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),
    awful.key({ modkey            } , "e", function () awful.util.spawn_with_shell("exec ~/bin/switch-monitor --hdmi") end), -- external
    awful.key({ modkey, "Shift"   } , "e", function () awful.util.spawn_with_shell("exec ~/bin/switch-monitor --default") end), -- external off
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey,           } , "c",
        function ()
            local process = io.popen("cat /proc/$(xdotool getwindowpid $(xdotool getwindowfocus))/comm"):read("*all")
            if process == "urxvtd" then
                awful.util.spawn_with_shell("xdotool getwindowfocus key --window %1 alt+c")
            else
                awful.util.spawn_with_shell("xdotool getwindowfocus key --window %1 ctrl+c")
            end
        end ,
        {description = "mac like copy", group = "client"}),
    awful.key({ modkey,           } , "v",
        function ()
            process = io.popen("cat /proc/$(xdotool getwindowpid $(xdotool getwindowfocus))/comm"):read("*all")
            if process == "urxvtd" then
                awful.util.spawn_with_shell("xdotool getwindowfocus key --window %1 ctrl+v")
            else
                awful.util.spawn_with_shell("xdotool getwindowfocus key --window %1 ctrl+v")
            end
        end ,
        {description = "mac like paste", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
        },
        class = {
          "mpv",
          "feh",
          "XVkbd",
          "ncmpcpp",
          "Arandr",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Wpa_gui",
          "pinentry",
          "veromix",
          "xtightvncviewer"},

        name = {
          "Event Tester",  -- xev.
          "xzoom x2"
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},
    { rule = { class = "URxvt"       } , properties = { border_width = 0 }  } ,
    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
