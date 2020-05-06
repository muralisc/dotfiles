-- This function returns a formatted string with the current battery status. It
-- can be used to populate a text widget in the awesome window manager. Based
-- on the "Gigamo Battery Widget" found in the wiki at awesome.naquadah.org

local awful = require("awful")
local naughty = require("naughty")
local beautiful = require("beautiful")
local old = 0

function batteryInfo(adapter)
    percent = ""
    icon = ""
    seconds = 0
    local fcur = io.open("/sys/class/power_supply/"..adapter.."/charge_now")
    local fcap = io.open("/sys/class/power_supply/"..adapter.."/charge_full")
    local fdes = io.open("/sys/class/power_supply/"..adapter.."/charge_full_design")
    local fsta = io.open("/sys/class/power_supply/"..adapter.."/status")
    -- if battery doesnt exist; return {{{
    if( fcur == nil ) then
        icon = "⚡"
        battery = "A/C"
        percent = ""
        health = "";
        return "| "..icon..battery..percent..health.." "
    end
    -- }}}
    local cur = fcur:read()
    local cap = fcap:read()
    local des = fdes:read()
    local sta = fsta:read()
    fcur:close()
    fcap:close()
    fdes:close()
    fsta:close()
    health = string.format( "<span color='#00B000'> H: %2.0f%% </span>" , math.floor( cap * 100 / des ) )
    if sta == "Full" then
        battery = "A/C"
        icon = " "
        percent = " "
    elseif sta:match("Charging") then
        battery = math.floor(cur * 100 / cap)
        icon = "⚡"
        percent = "% "
    elseif sta:match("Discharging") then
        battery = math.floor(cur * 100 / cap)
        icon = ""
        seconds = cur*10/(old - cur)
        percent = string.format("%% %2.2f hrz rem ", seconds/(60*60))
        old = cur
--{{{   Low battery if condition
        if tonumber(battery) < 30 then
            naughty.notify({ title    = "Battery Warning"
            , text     = "Battery low!".."  "..battery..percent.."  ".."left!"
            , timeout  = 5
            , position = "top_right"
            , fg       = beautiful.fg_focus
            , bg       = beautiful.bg_focus
        })
        end
--}}}
--{{{   Critical Battery suspend if condition
        if tonumber(battery) < 15 then
            awful.util.spawn_with_shell("systemctl suspend")
        end
--}}}
    end
    return "| "..icon..battery..percent..health.." "
end
