pro cut_profile,namein,nameout,threshold

read_profile,namein,in,x,si,sq,su,sv

si(where(si lt threshold))=-10.

write_profile,namein,in,x,si,sq,su,sv

return
end