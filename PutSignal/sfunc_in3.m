function ret = sfunc_in3(~)
    global extIn3; % Instance of Java or Scala Class
    ret = extIn3.get();
end

