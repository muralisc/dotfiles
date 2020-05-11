#!./bin/lua

local driver = require('luasql.sqlite3')
local env = driver.sqlite3()
local db = env:connect('mpvdb.sqlite')

local results = db:execute[[
  SELECT * FROM generic
]]

local key,value,perc = results:fetch()
if value == nil then
    print("no results found")
else
    while key do
        print(key ..': '.. value .. ': ' .. perc)
        key,value,perc = results:fetch()
    end
end

results:close()
-- db:execute[[
--   UPDATE generic SET value = "murali" WHERE key = "nome"
-- ]]
db:close()
env:close()