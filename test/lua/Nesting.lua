-- Testing various statements and nesting of those
local my_arr = { 1, 3, 5, 10, 23 }
local i = 1
local some_value = 0

::beginning_flow::
while i < 6 do
   if my_arr[i] % 2 == 0 then
      some_value = some_value + 1
      break
   end

   local j = 0
   repeat
      j = j + 1
   until j == 10
end



if 10 < some_value then
   goto beginning_flow
end
