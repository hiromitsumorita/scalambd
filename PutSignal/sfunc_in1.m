function ret = sfunc_in1(~)
    global extIn1; % Instance of Java or Scala Class
    ret = extIn1.get();
end

