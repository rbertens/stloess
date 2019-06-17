      subroutine lowesd(versio,iv,liv,lv,v,d,n,f,ideg,nvmax,setlf)
      logical setlf
      integer bound,d,execnt,i,i1,i2,ideg,j,liv,lv,n,ncmax,nf,nvmax,vc,v
     $ersio
      integer iv(liv)
      real f
      real v(lv)
      external ehg182
      integer ifloor
      external ifloor
      save execnt
      data execnt /0/
c     version -> versio
      execnt=execnt+1
      if(.not.(versio.eq.106))then
         call ehg182(100)
      end if
      iv(28)=171
      iv(2)=d
      iv(3)=n
      vc=2**d
      iv(4)=vc
      if(.not.(0.lt.f))then
         call ehg182(120)
      end if
      nf=min(n,ifloor(n*f))
      iv(19)=nf
      iv(20)=1
      if(ideg.eq.0)then
         i1=1
      else
         if(ideg.eq.1)then
            i1=d+1
         else
            if(ideg.eq.2)then
               i1=float((d+2)*(d+1))/2.
            end if
         end if
      end if
      iv(29)=i1
      iv(21)=1
      iv(14)=nvmax
      ncmax=nvmax
      iv(17)=ncmax
      iv(30)=0
      iv(32)=ideg
      if(.not.(ideg.ge.0))then
         call ehg182(195)
      end if
      if(.not.(ideg.le.2))then
         call ehg182(195)
      end if
      iv(33)=d
      do 3 i2=41,49
         iv(i2)=ideg
    3 continue
      iv(7)=50
      iv(8)=iv(7)+ncmax
      iv(9)=iv(8)+vc*ncmax
      iv(10)=iv(9)+ncmax
      iv(22)=iv(10)+ncmax
c     initialize permutation
      j=iv(22)-1
      do 4 i=1,n
         iv(j+i)=i
    4 continue
      iv(23)=iv(22)+n
      iv(25)=iv(23)+nvmax
      if(setlf)then
         iv(27)=iv(25)+nvmax*nf
      else
         iv(27)=iv(25)
      end if
      bound=iv(27)+n
      if(.not.(bound-1.le.liv))then
         call ehg182(102)
      end if
      iv(11)=50
      iv(13)=iv(11)+nvmax*d
      iv(12)=iv(13)+(d+1)*nvmax
      iv(15)=iv(12)+ncmax
      iv(16)=iv(15)+n
      iv(18)=iv(16)+nf
      iv(24)=iv(18)+iv(29)*nf
      iv(34)=iv(24)+(d+1)*nvmax
      if(setlf)then
         iv(26)=iv(34)+(d+1)*nvmax*nf
      else
         iv(26)=iv(34)
      end if
      bound=iv(26)+nf
      if(.not.(bound-1.le.lv))then
         call ehg182(103)
      end if
      v(1)=f
      v(2)=0.05
      v(3)=0.
      v(4)=1.
      return
      end
