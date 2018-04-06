# Estimated Plasma Volume
# weight in kg, hct in %
# Kaplan aa. A simple and accurate method for prescribing plasma exchange. Trans Am Soc Artif Intern Organs 1990
epv <- function(weight,hct) {
  if(hct>1)
    hct<-hct/100
  return(0.065*weight*(1-hct))
}

# Post treatment concentration
# pre_conc pre treatment concentration, ve volume exchanged, epv estimated plasma volume
post_conc <- function(pre_conc,ve,epv,mw) {
  return(pre_conc*exp(-(ve/epv)*2/log10(mw)))
}

# Return blood concentration
# inlet_con concentration in inlet blood, f_factor filtration factor in %, removed T if the substance is removed
ret_conc <- function(inlet_conc,f_factor,removed) {
  if(removed)
    return(inlet_conc-inlet_conc*f_factor)
  else
    return(inlet_conc+inlet_conc*f_factor)
}

# Treatment duration (min)
# ve volume to exchange in mL, b_flow blood flow in mL/min, f_factor filtration factor in % max 20%
t_time <- function(ve,b_flow,f_factor) {
  return(round(ve/(b_flow*f_factor)))
}

# Total body water in L
# Watson : 	Male TBW = 	2.447 - (0.09156 x age) + (0.1074 x height) + (0.3362 x weight)
#           Female TBW = 	-2.097 + (0.1069 x height) + (0.2466 x weight)
#   age in years, height in cm, weight in kg, sex is either "M" or "F"
# Chertow's Bioelectrical Impedance:
#TBW = 	ht x (0.0186104 x wt + 0.12703384) + wt x (0.11262857 x male + 0.00104135 x age - 0.00067247 x wt - 0.04012056)
#- age x (0.03486146 x male + 0.07493713) - male x 1.01767992 + diabetes x 0.57894981
tbw <- function(age,sex,weight,height) {
  if(toupper(sex)=="M")
    return(2.447 - (0.09156 * age) + (0.1074 * height) + (0.3362 * weight))
  if(toupper(sex)=="F")
    return(-2.097 + (0.1069 * height) + (0.2466 * weight))
  return("Sesso non corretto")
}

# Extra cellular volume in L
# estimated as 1/3 of total body water
ecv <- function(tbw) {
  return(tbw/3)
}

# Extra vascular volume in L
# estimated as ecv extra cellular volume - ivv intra vascular volume
evv <- function(ecv,ivv){
  return(ecv-ivv)
}

# Body composition
param <- function(w,h,a,sex,hct,perc_i,c_iv){
  iv<-epv(weight = w,hct=hct/100)*1000
  ev<-evv(ecv(tbw(age=a,sex=sex,weight = w,height = h)),ivv=iv/1000)*1000
  # mg
  m_iv<-c_iv*iv
  m_ev<-(m_iv/perc_i)*(1-perc_i)
  c_ev<-m_ev/ev
  return(data.table(c_iv,m_iv,iv,c_ev,m_ev,ev))
}

# plasmapheresis treatment slice
pex <- function(t,v_exc,iv,b_flow,f_factor,ev,c_iv_prec,m_iv_prec,c_ev_prec,m_ev_prec,km,
                dest,prod,perc_i,mw){
  t_pex<-t_time(ve = v_exc,b_flow = b_flow,f_factor = f_factor)
  if(t<=t_pex){
    c_iv<-post_conc(pre_conc = c_iv_prec,ve=f_factor*b_flow,epv=iv,mw=mw)
  }else{
    c_iv<-c_iv_prec
  }
  #c_i[t]<-c_i[t]
  m_iv<-c_iv*iv+((perc_i)-m_iv_prec/(m_iv_prec+m_ev_prec))*km*m_ev_prec-dest*iv/(iv+ev)+prod*iv/(iv+ev)
  c_iv<-m_iv/iv
  c_ev<-c_ev_prec
  m_ev<-c_ev*ev-((perc_i)-m_iv_prec/(m_iv_prec+m_ev_prec))*km*m_ev_prec-dest*ev/(iv+ev)+prod*ev/(iv+ev)
  c_ev<-m_ev/ev
  return(data.table(c_iv,m_iv,iv,c_ev,m_ev,ev,b_flow,f_factor,km,prod,dest,t_pex,v_exc))
}
