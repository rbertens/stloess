      subroutine ehg196(tau,d,f,trl)
      integer d,dka,dkb,execnt,tau
      real alpha,f,trl,trla,trlb
      external ehg197
      save execnt
      data execnt /0/
      execnt=execnt+1
      call ehg197(1,tau,d,f,dka,trla)
      call ehg197(2,tau,d,f,dkb,trlb)
      alpha=float(tau-dka)/float(dkb-dka)
      trl=(1-alpha)*trla+alpha*trlb
      return
      end
