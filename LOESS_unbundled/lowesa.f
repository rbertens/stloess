      subroutine lowesa(trl,n,d,tau,nsing,delta1,delta2)
      integer d,dka,dkb,execnt,n,nsing,tau
      real alpha,d1a,d1b,d2a,d2b,delta1,delta2,trl
      external ehg141
      save execnt
      data execnt /0/
      execnt=execnt+1
      call ehg141(trl,n,1,tau,d,nsing,dka,d1a,d2a)
      call ehg141(trl,n,2,tau,d,nsing,dkb,d1b,d2b)
      alpha=float(tau-dka)/float(dkb-dka)
      delta1=(1-alpha)*d1a+alpha*d1b
      delta2=(1-alpha)*d2a+alpha*d2b
      return
      end
