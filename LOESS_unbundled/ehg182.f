      subroutine ehg182(i)
      integer i
      if(i.eq.100) print *,'wrong version number in lowesd.  Probably ty
     +po in caller.'
      if(i.eq.101) print *,'d>dMAX in ehg131.  Need to recompile with in
     +creased dimensions.'
      if(i.eq.102) print *,'liv too small.   (Discovered by lowesd)'
      if(i.eq.103) print *,'lv too small.    (Discovered by lowesd)'
      if(i.eq.104) print *,'alpha too small.  fewer data values than deg
     +rees of freedom.'
      if(i.eq.105) print *,'k>d2MAX in ehg136.  Need to recompile with i
     +ncreased dimensions.'
      if(i.eq.106) print *,'lwork too small'
      if(i.eq.107) print *,'invalid value for kernel'
      if(i.eq.108) print *,'invalid value for ideg'
      if(i.eq.109) print *,'lowstt only applies when kernel=1.'
      if(i.eq.110) print *,'not enough extra workspace for robustness ca
     +lculation'
      if(i.eq.120) print *,'zero-width neighborhood. make alpha bigger'
      if(i.eq.121) print *,'all data on boundary of neighborhood. make a
     +lpha bigger'
      if(i.eq.122) print *,'extrapolation not allowed with blending'
      if(i.eq.123) print *,'ihat=1 (diag L) in l2fit only makes sense if
     + z=x (eval=data).'
      if(i.eq.171) print *,'lowesd must be called first.'
      if(i.eq.172) print *,'lowesf must not come between lowesb and lowe
     +se, lowesr, or lowesl.'
      if(i.eq.173) print *,'lowesb must come before lowese, lowesr, or l
     +owesl.'
      if(i.eq.174) print *,'lowesb need not be called twice.'
      if(i.eq.180) print *,'nv>nvmax in cpvert.'
      if(i.eq.181) print *,'nt>20 in eval.'
      if(i.eq.182) print *,'svddc failed in l2fit.'
      if(i.eq.183) print *,'didnt find edge in vleaf.'
      if(i.eq.184) print *,'zero-width cell found in vleaf.'
      if(i.eq.185) print *,'trouble descending to leaf in vleaf.'
      if(i.eq.186) print *,'insufficient workspace for lowesf.'
      if(i.eq.187) print *,'insufficient stack space'
      if(i.eq.188) print *,'lv too small for computing explicit L'
      if(i.eq.191) print *,'computed trace L was negative; something is 
     +wrong!'
      if(i.eq.192) print *,'computed delta was negative; something is wr
     +ong!'
      if(i.eq.193) print *,'workspace in loread appears to be corrupted'
      if(i.eq.194) print *,'trouble in l2fit/l2tr'
      if(i.eq.195) print *,'only constant, linear, or quadratic local mo
     +dels allowed'
      if(i.eq.196) print *,'degree must be at least 1 for vertex influen
     +ce matrix'
      if(i.eq.999) print *,'not yet implemented'
      print *,'Assert failed, error code ',i
      stop
      end
      subroutine ehg183(s,i,n,inc)
      character*(*) s
      integer n, inc, i(inc,n), j
      print *,s,(i(1,j),j=1,n)
      end
      subroutine ehg184(s,x,n,inc)
      character*(*) s
      integer n, inc, j
      real x(inc,n)
      print *,s,(x(1,j),j=1,n)
      end
