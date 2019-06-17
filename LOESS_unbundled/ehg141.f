      subroutine ehg141(trl,n,deg,k,d,nsing,dk,delta1,delta2)
      integer d,deg,dk,k,n,nsing
      external ehg176
      real ehg176
      real corx,delta1,delta2,trl,z
      real c(48), c1, c2, c3, c4
c     coef, d, deg, del
      data c /
     $ .2971620,.3802660,.5886043
     $,.4263766,.3346498,.6271053
     $,.5241198,.3484836,.6687687
     $,.6338795,.4076457,.7207693
     $,.1611761,.3091323,.4401023
     $,.2939609,.3580278,.5555741
     $,.3972390,.4171278,.6293196
     $,.4675173,.4699070,.6674802
     $,.2848308,.2254512,.2914126
     $,.5393624,.2517230,.3898970
     $,.7603231,.2969113,.4740130
     $,.9664956,.3629838,.5348889
     $,.2075670,.2822574,.2369957
     $,.3911566,.2981154,.3623232
     $,.5508869,.3501989,.4371032
     $,.7002667,.4291632,.4930370
     $ /
      if(deg.eq.0) dk=1
      if(deg.eq.1) dk=d+1
      if(deg.eq.2) dk=float((d+2)*(d+1))/2.
      corx=sqrt(k/float(n))
      z=(sqrt(k/trl)-corx)/(1-corx)
      if(nsing .eq. 0 .and. 1 .lt. z)
     $   call ehg184('Chernobyl! trL<k',trl,1,1)
      if(z .lt. 0) call ehg184('Chernobyl! trL>n',trl,1,1)
      z=min(1.0,max(0.0,z))
      c4=exp(ehg176(z))
      i=1+3*(min(d,4)-1+4*(deg-1))
      if(d.le.4)then
         c1=c(i)
         c2=c(i+1)
         c3=c(i+2)
      else
         c1=c(i)+(d-4)*(c(i)-c(i-3))
         c2=c(i+1)+(d-4)*(c(i+1)-c(i-2))
         c3=c(i+2)+(d-4)*(c(i+2)-c(i-1))
      endif
      delta1=n-trl*exp(c1*z**c2*(1-z)**c3*c4)
      i=i+24
      if(d.le.4)then
         c1=c(i)
         c2=c(i+1)
         c3=c(i+2)
      else
         c1=c(i)+(d-4)*(c(i)-c(i-3))
         c2=c(i+1)+(d-4)*(c(i+1)-c(i-2))
         c3=c(i+2)+(d-4)*(c(i+2)-c(i-1))
      endif
      delta2=n-trl*exp(c1*z**c2*(1-z)**c3*c4)
      return
      end
