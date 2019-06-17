      subroutine lowesb(xx,yy,ww,diagl,infl,iv,liv,lv,wv)
      logical infl,setlf
      integer execnt
      integer iv(*)
      real trl
      real diagl(*),wv(*),ww(*),xx(*),yy(*)
      external ehg131,ehg182,ehg183
      integer ifloor
      external ifloor
      save execnt
      data execnt /0/
      execnt=execnt+1
      if(.not.(iv(28).ne.173))then
         call ehg182(174)
      end if
      if(iv(28).ne.172)then
         if(.not.(iv(28).eq.171))then
            call ehg182(171)
         end if
      end if
      iv(28)=173
      if(infl)then
         trl=1.
      else
         trl=0.
      end if
      setlf=(iv(27).ne.iv(25))
      call ehg131(xx,yy,ww,trl,diagl,iv(20),iv(29),iv(3),iv(2),iv(5),iv(
     $17),iv(4),iv(6),iv(14),iv(19),wv(1),iv(iv(7)),iv(iv(8)),iv(iv(9)),
     $iv(iv(10)),iv(iv(22)),iv(iv(27)),wv(iv(11)),iv(iv(23)),wv(iv(13)),
     $wv(iv(12)),wv(iv(15)),wv(iv(16)),wv(iv(18)),ifloor(iv(3)*wv(2)),wv
     $(3),wv(iv(26)),wv(iv(24)),wv(4),iv(30),iv(33),iv(32),iv(41),iv(iv(
     $25)),wv(iv(34)),setlf)
      if(iv(14).lt.iv(6)+float(iv(4))/2.)then
         call ehg183('Warning. k-d tree limited by memory; nvmax=',iv(14
     $),1,1)
      else
         if(iv(17).lt.iv(5)+2)then
            call ehg183('Warning. k-d tree limited by memory. ncmax=',iv
     $(17),1,1)
         end if
      end if
      return
      end
