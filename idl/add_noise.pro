@ read_profile
@ write_profile

pro add_noise,file_name,noise,seed

read_profile,file_name,in,lam,i,q,u,v
n=n_elements(i)

;seed=47.

n_i=randomn(seed,n,/normal)*noise
n_q=randomn(seed,n,/normal)*noise
n_u=randomn(seed,n,/normal)*noise
n_v=randomn(seed,n,/normal)*noise


i=i+n_i
q=q+n_q
u=u+n_u
v=v+n_v

write_profile,'noisy-'+file_name,in,lam,i,q,u,v

end