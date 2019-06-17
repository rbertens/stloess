      subroutine ehg197(deg,tau,d,f,dk,trl)
      integer d,deg,dk,tau
      real trl, f
      dk = 0
      if(deg.eq.1) dk=d+1
      if(deg.eq.2) dk=float((d+2)*(d+1))/2.
      g1 = (-0.08125*d+0.13)*d+1.05
      trl = dk*(1+max(0.,(g1-f)/f))
      return
      end
