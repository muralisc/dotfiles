local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")

prev_id = 0
function update_music( command )
   awful.util.spawn_with_shell( command )
   nev = naughty.notify({
          text     = ""
                .. string.format("%s" , io.popen("mpc | head -2"):read("*all") )
        , timeout  = 3
        , position = "top_right"
        , font     = "Ubuntu Mono 16"
        , replaces_id = prev_id
    })
   prev_id = nev.id;
end
