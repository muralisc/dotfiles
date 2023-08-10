-- Copyright (c) 2020, Eisa AlAwadhi
-- License: BSD 2-Clause License

-- Creator: Eisa AlAwadhi
-- Project: SmartHistory
-- Version: 1.5.2

-- local driver = require("luasql.sqlite3")
-- local env = driver.sqlite3()
local dbfile = (os.getenv("APPDATA") or os.getenv("HOME") .. "/.config") .. "/mpv/mpvdb.sqlite"
-- local db = env:connect(dbfile)

local utils = require("mp.utils")
local filePath = ""
local time = 0
local pause_start = 0
local pause_duraton = 0
local play_duration = 0

mp.register_event("file-loaded", function()
  filePath = utils.join_path(mp.get_property("working-directory"), mp.get_property("path"))
  total_duration = mp.get_property_number("duration")
  print("file-loaded, duration:", total_duration)

  pause_start = session_start

  command = "sqlite3 "
    .. dbfile
    .. " 'CREATE TABLE IF NOT EXISTS generic ( path varchar(550), timestamp_val varchar(150), percentage varchar(150))'"
  local handle = io.popen(command)
  local result = handle:read("*a")
  handle:close()
  print("Printing result" .. result)
end)

mp.register_event("playback-restart", function()
  session_start = os.time(os.date("!*t"))
  pause_start = os.time(os.date("!*t"))
  time = math.floor(mp.get_property_number("time-pos") or 0)
  print("playback-restart at position", time)
end)

function on_pause_change(name, value)
  if value == true then
    pause_start = os.time(os.date("!*t"))
    print("pause at time ", pause_start)
  else
    pause_end = os.time(os.date("!*t"))
    print("unpause: pause duration:", (pause_end - pause_start))
    pause_duraton = pause_duraton + (pause_end - pause_start)
    print("unpause: total pause time", pause_duraton)
  end
end
mp.observe_property("pause", "bool", on_pause_change)

mp.register_event("end-file", function()
  local historyLog = (os.getenv("APPDATA") or os.getenv("HOME") .. "/.config") .. "/mpv/mpvHistory.log"
  local historyLogAdd = io.open(historyLog, "a+")

  session_end = os.time(os.date("!*t"))
  play_duration = play_duration + (session_end - session_start)

  seconds = 0
  if filePath ~= nil then
    historyLogAdd:write(("[%s] %s\n"):format(os.date("%d/%b/%y %X"), filePath .. " |time=" .. tostring(seconds)))
    seconds = 0
    time = 0
    historyLogAdd:close()

    print(
      "end-file play_duration: "
        .. play_duration
        .. " Duration: "
        .. total_duration
        .. " pause_duration:"
        .. pause_duraton
    )
    print(("percent played %s\n"):format(play_duration * 100 / total_duration))
    -- path , last played time, percent_played
    local query = ("sqlite3 %s \"INSERT INTO generic VALUES('%s', '%d', '%s')\""):format(
      dbfile,
      filePath,
      os.time(os.date("!*t")),
      play_duration * 100 / total_duration
    )
    print(query)
    local handle = io.popen(query)
    local result = handle:read("*a")
    handle:close()
    print(result)
  end
end)
