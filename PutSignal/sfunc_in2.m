function ret = sfunc_in2(~)
    global extIn2; % Instance of Java or Scala Class
    ret = extIn2.get();
end

