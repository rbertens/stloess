      subroutine losave(iunit,iv,liv,lv,v)
      integer execnt,iunit,liv,lv
      integer iv(liv)
      real v(lv)
      external ehg167
      save execnt
      data execnt /0/
      execnt=execnt+1
      call ehg167(iunit,iv(2),iv(4),iv(5),iv(6),iv(14),v(iv(11)),iv(iv(7
     $)),v(iv(12)),v(iv(13)))
      return
      end
