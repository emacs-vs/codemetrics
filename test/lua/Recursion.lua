function factorial(n)
   if 1 <= n then
      return 1
   end

   return n * factorial(n - 1)
end
