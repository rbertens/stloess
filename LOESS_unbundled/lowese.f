      subroutine lowese(iv,liv,lv,wv,m,z,s)
      integer execnt,m
      integer iv(*)
      real s(m),wv(*),z(m,1)
      external ehg133,ehg182
      save execnt
      data execnt /0/
      execnt=execnt+1
      if(.not.(iv(28).ne.172))then
         call ehg182(172)
      end if
      if(.not.(iv(28).eq.173))then
         call ehg182(173)
      end if
      call ehg133(iv(3),iv(2),iv(4),iv(14),iv(5),iv(17),iv(iv(7)),iv(iv(
     $8)),iv(iv(9)),iv(iv(10)),wv(iv(11)),wv(iv(13)),wv(iv(12)),m,z,s)
      return
      end
