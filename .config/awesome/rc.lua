-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

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
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("~/.config/awesome/theme.lua")

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

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,             --RESIZE dont work
    awful.layout.suit.fair.horizontal,  --RESIZE dont work
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.centered(beautiful.wallpaper, s, "black")
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
    names  = {
                        -- "1"
                 " MA1N",
                      -- 2
                 " B2OWSE",
                      -- 3
                 " T3RMINALS",
                      -- 4
                 " re4dings",
                      -- 5
                 " Mail5",
                      -- 6
                 " 6heats",
                      -- 7
                 " 7he rcs",
                      -- 8
                 " M8Ns",
                      -- 9
                 " CODIN9",
            },
    layout = {
                layouts[1],    --floating,
                layouts[2],    --tile,
                layouts[2],    --tile,
                layouts[2],    --tile,
                layouts[2],    --tile,
                layouts[2],    --tile,
                layouts[2],    --tile,
                layouts[2],    --tile,
                layouts[2],    --tile,
                -- layouts[2],    --tile,
                -- layouts[3],    --tile.left
                -- layouts[4],    --tile.bottom,
                -- layouts[5],    --tile.top,
                -- layouts[6],    --fair,              --RESIZE dont work
                -- layouts[7],    --fair.horizontal,   --RESIZE dont work
                -- layouts[8],    --spiral,
                -- layouts[9],    --spiral.dwindle,
                -- layouts[10],   --max,
                -- layouts[11]   --max.fullscreen,
                -- layouts[12],   --magnifier
            }
        }
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Menu
require('freedesktop.menu')
menu_items = freedesktop.menu.new()
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal },
                                    { "shutdown", 'poweroff' },
                                    { "free-desktop",  menu_items}
                                  },
                          theme = { width = 150,}
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({
                                                      theme = { width = 250 }
                                                  })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))
  local vicious = require("vicious")
  -- Top widgets:
  require("volume")
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
                        cpuwidget_t:set_text(""
                        .. string.format("%3d%% Cpu 1\n",args[2])
                        .. string.format("%3d%% Cpu 2\n",args[3])
                        .. string.format("%3d%% Cpu 3\n",args[4])
                        .. string.format("%3d%% Cpu 4",args[5])
                        )
                        return args[1]
                    end)

hddwidget = wibox.widget.textbox()
vicious.register(hddwidget, vicious.widgets.thermal, " <span color='#FFFF00'>$1°С </span>",37,"thermal_zone0")

-- Initialize widget
memwidget = awful.widget.progressbar()
-- Progressbar properties
memwidget:set_width(8)
memwidget:set_height(10)
memwidget:set_vertical(true)
memwidget:set_background_color("#494B4F")
memwidget:set_border_color(nil)
memwidget:set_color({ type = "linear", from = { 0, 0 }, to = { 10,0 }, stops = { {0, "#AECF96"}, {0.5, "#88A175"},
                    {1, "#FF5656"}}})
memwidget_t = awful.tooltip({ objects = { memwidget },})
-- Register widget
vicious.register(memwidget, vicious.widgets.mem,
                    function (widget, args)
                        memwidget_t:set_text(
                        string.format(" %4d %-5s \n" ,args[3],"total"    )..
                        string.format(" %4d %-5s \n" ,args[2],"Used"     )..
                        string.format(" %4d %-5s \n" ,args[4],"free"     )..
                        string.format(" %4d %-5s \n" ,args[6],"swap%"    )..
                        string.format(" %4d %-5s"    ,args[7],"swap "    )
                        )
                        return args[1]
                    end
                                                , 13)

wifiwidget = wibox.widget.textbox()
vicious.register(wifiwidget, vicious.widgets.wifi,
        function (widget, args)
            return string.format("<span color='#5882FA'>| %s %3s%% |</span>",args["{ssid}"], args["{linp}"])
        end
        , 3, "wlp2s0")

netgraph = awful.widget.graph()
netgraph:set_width(50)
netgraph:set_border_color("#000000")
netgraph:set_background_color("#494B4F")
netgraph:set_color("#FF5656")
netgraph:set_max_value(10)   -- 1000kbps
netgraph_t = awful.tooltip({ objects = { netgraph },})
vicious.cache(vicious.widgets.net)
vicious.register(netgraph, vicious.widgets.net,
                    function (widget, args)
                        netgraph_t:set_text(""..
                        string.format("%2.3f GB Recieved\n"    ,args["{wlp2s0 rx_gb}"])..
                        string.format("%2.3f GB Txn-ed\n"    ,args["{wlp2s0 tx_gb}"])..
                        string.format("%2.3f Mbps down\n"  ,args["{wlp2s0 down_mb}"])..
                        string.format("%2.3f Mbps up"    ,args["{wlp2s0 up_mb}"])..
                        ""
                        )
                        return args["{wlp2s0 down_kb}"]
                    end)

local battery = require("battery")
batterywidget = wibox.widget.textbox()
batterywidget_timer = timer({timeout = 10})
batterywidget_timer:connect_signal("timeout", function()
    batterywidget:set_text(batteryInfo("BAT0").."|")
end)
batterywidget_timer:start()

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top",screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    -- left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])
    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(cpuwidget)
    right_layout:add(memwidget)
    right_layout:add(netgraph)
    right_layout:add(hddwidget)
    right_layout:add(wifiwidget)
    right_layout:add(batterywidget)
    right_layout:add(mytextclock)
    right_layout:add(volume_widget)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    -- layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
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

    awful.key({}, "XF86AudioRaiseVolume" ,
        function ()
            awful.util.spawn("amixer sset Master 10%+")
        end),
    awful.key({}, "XF86AudioLowerVolume" ,
        function ()
            awful.util.spawn("amixer sset Master 10%-")
        end),
    awful.key({}, "XF86AudioMute",
        function ()
            awful.util.spawn("amixer sset 'Master',0 toggle")
        end),
    awful.key({}, "XF86AudioPlay",
        function ()
            awful.util.spawn("xmms2 toggle")
        end),
    awful.key({ "Control", "Mod1" }, "Left",
        function ()
            awful.util.spawn("xmms2 prev")
        end),
    awful.key({ "Control", "Mod1" }, "Right",
        function ()
            awful.util.spawn("xmms2 next")
        end),

    awful.key({}, "XF86MonBrightnessUp" ,
        function ()
            awful.util.spawn("xbacklight +10")
        end),
    awful.key({}, "XF86MonBrightnessDown" ,
        function ()
            awful.util.spawn("xbacklight -10")
        end),
    awful.key({}, "XF86Calculator" ,
        function ()
            awful.util.spawn("xbacklight +10")
        end),
    awful.key({}, "XF86Sleep" ,
        function ()
            awful.util.spawn("xbacklight -10")
        end),

    awful.key({}, "XF86Mail" ,
        function ()
            awful.util.spawn("thunderbird")
        end),
    awful.key({}, "XF86HomePage" ,
        function ()
            awful.util.spawn("firefox")
        end),

    awful.key({}, "Print" ,
        function ()
            awful.util.spawn_with_shell("sleep 0.5 && scrot -s")
        end),
    awful.key({}, "Scroll_Lock" ,
        function ()
            awful.util.spawn_with_shell("exec ~/.config/awesome/lockScript.sh")
        end),
    awful.key({}, "Pause" ,
        function ()
            awful.util.spawn("pcmanfm")
        end),


    awful.key({ "Mod1" }, "Tab",
        function ()
            awful.menu.clients( { theme = { font="zekton bold 14"; width = 800; height=30 } }) 
        end),  --ESCAPE to close

    awful.key({ modkey }, "Next",
        function ()
            awful.client.moveresize( 0,  0, 40, 0)
        end),
    awful.key({ modkey, "Shift" }, "Next",
        function ()
            awful.client.moveresize( 0,  0,  -40, 0)
        end),
    awful.key({ modkey }, "Prior",
        function ()
            awful.client.moveresize( 0, 0, 0,  40)
        end),
    awful.key({ modkey, "Shift" }, "Prior",
        function ()
            awful.client.moveresize( 0, 0, 0,  -40)
        end),
    awful.key({ modkey }, "Down",
        function ()
            awful.client.moveresize(  0,  20,   0,   0)
        end),
    awful.key({ modkey }, "Up",
        function ()
            awful.client.moveresize(  0, -20,   0,   0)
        end),
    awful.key({ modkey }, "Left",
        function ()
            awful.client.moveresize(-20,   0,   0,   0)
        end),
    awful.key({ modkey }, "Right",
        function ()
            awful.client.moveresize( 20,   0,   0,   0)
        end),

    awful.key({ modkey,}, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end), -- focus the next client
    awful.key({ modkey, "Shift"   }, "j",
        function ()
            awful.client.swap.byidx(  1)
        end), --move clinet clockwise
    awful.key({ modkey, "Control" }, "j",
        function ()
            awful.screen.focus_relative( 1)
        end), -- what??
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey, "Shift"   }, "k",
        function ()
            awful.client.swap.byidx( -1)
        end),
    awful.key({ modkey, "Control" }, "k",
        function ()
            awful.screen.focus_relative(-1)
        end), -- what??
    awful.key({ "Control", "Mod1" }, "h",   awful.tag.viewprev       ),
    awful.key({ "Control", "Mod1" }, "l",  awful.tag.viewnext       ),

    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "w",
        function ()
            mymainmenu:show()
        end),
    -- Layout manipulation
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),
    -- Standard program
    awful.key({ modkey,           }, "Return",
        function ()
            awful.util.spawn(terminal)
        end),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),
    awful.key({ modkey, "Control" }, "r", awesome.restart),

    awful.key({ modkey,           }, "l",
        function ()
            awful.tag.incmwfact( 0.05)
        end), -- RESIZE
    awful.key({ modkey, "Shift"   }, "l",
        function ()
            awful.client.incwfact(-0.05)
        end), -- RESIZE
    -- awful.key({ modkey, "Shift"   }, "l",
    --  function ()
    --  awful.tag.incnmaster(-1)
    --  end),
    awful.key({ modkey, "Control" }, "l",
        function ()
            awful.tag.incncol(-1)
        end),
    awful.key({ modkey,           }, "h",
        function ()
            awful.tag.incmwfact(-0.05)
        end), -- RESIZE
    awful.key({ modkey, "Shift"   }, "h",
        function ()
            awful.client.incwfact( 0.05)
        end), -- RESISE
    -- awful.key({ modkey, "Shift"   }, "h",
    --  function ()
    --  awful.tag.incnmaster( 1)
    --  end),
    awful.key({ modkey, "Control" }, "h",
        function ()
            awful.tag.incncol( 1)
        end),
    awful.key({ modkey,           }, "space",
        function ()
            awful.layout.inc(layouts,  1)
        end),
    awful.key({ modkey, "Shift"   }, "space",
        function ()
            awful.layout.inc(layouts, -1)
        end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     
        function () 
            mypromptbox[mouse.screen]:run() 
        end),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end),
    awful.key({ modkey }, "b",              -- wibox visibility toggle
        function () 
            mywibox[mouse.screen].visible = not mywibox[mouse.screen].visible 
        end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      
        function (c) 
            c.ontop = not c.ontop            
        end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
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
                     buttons = clientbuttons } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    { rule = { class = "Thunderbird" },
      properties = { tag = tags[1][5] } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    { rule = { class = "Google-chrome" },
    callback = function(c) c:tags({awful.tag.selected(1), tags[1][2]}) end},
    { rule = { class = "Firefox" },
    callback = function(c) c:tags({awful.tag.selected(1), tags[1][2]}) end},
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
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

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)
client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
