!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-11-02
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTestPath.f90 

#include "criTest.fpp"

module criTestPath
use criPath
use criAlgorithm
implicit none
private

public criTestPath_main

contains
      
      !> main driver routine of criTestPath      
      logical function criTestPath_main() result(stat)
      implicit none
      
            stat = test_basename() 
            
            stat = test_stripExt()
            
            stat = test_splitExt()
            
      end function


      
      logical function test_basename()
      implicit none
      character(len=max_pathlen)    :: path
      !
            test_basename = .false.
            ! Test:
            _TEST('empty path, string',basename('') == '')

            ! Test:
            path = ''
            _TEST('empty path, path',basename(path) == '')

            ! Test:
            _TEST('blank path, string',basename('   ') == '')
            
            ! Test:
            _TEST('root only, string',basename(pathsep) == pathsep)

            ! Test:
            _TEST('root only "'//pathsep//'", path',basename(replaceAll('/','/',pathsep)) == pathsep)

            ! Test:
            path = replaceAll('/path1/path2/basename','/',pathsep)
            _TEST('canonical path "'//trim(path)//'" (file)',basename(path) == 'basename')
            
            ! Test:
            path = replaceAll('/path1/path2/basename/','/',pathsep)
            _TEST('canonical path "'//trim(path)//'" (dir)',basename(path) == 'basename')

            ! Test:
            path = replaceAll('/basename','/',pathsep)
            _TEST('canonical path "'//trim(path)//'" (file)',basename(path) == 'basename')

            ! Test:
            path = replaceAll('basename/','/',pathsep)
            _TEST('relative path "'//trim(path)//'" (dir)',basename(path) == 'basename')

            ! Test:
            path = replaceAll('path/basename','/',pathsep)
            _TEST('relative path "'//trim(path)//'" (file)',basename(path) == 'basename')

            ! Test:
            path = replaceAll('path/basename/','/',pathsep)
            _TEST('relative path "'//trim(path)//'" (dir)',basename(path) == 'basename')

            ! Test:
            path = replaceAll('path1/path2/basename','/',pathsep)
            _TEST('relative path "'//trim(path)//'" (file)',basename(path) == 'basename')

            test_basename = .true.
      !      
      end function
      
      
      logical function test_stripExt()
      implicit none
      character(len=max_pathlen)    :: path, path_res
      !
            test_stripExt = .false.
            ! Test:
            _TEST('stripExt, empty path, string',stripExt('') == '')
            
            ! Test:
            _TEST('stripExt, filename that starts with dot',stripExt('.ext') == '.ext')

            ! Test:
            path = pathsep // '.ext'
            path_res = path
            _TEST('stripExt, filename that starts with dot, root',stripExt(path) == path_res)

            
            ! Test:
            _TEST('stripExt, file, no extension',stripExt('filename') == 'filename')

            ! Test:
            _TEST('stripExt, file with extension',stripExt('filename.ext') == 'filename')
            
            ! Test:
            path = pathsep // 'filename'
            path_res = path
            _TEST('stripExt, file, no extension, root',stripExt(path) == path_res)

            
            ! Test:
            path = pathsep // 'filename.ext'
            path_res = pathsep // 'filename'
            _TEST('stripExt, file with extension, root',stripExt(path) == path_res)
            
            ! Test
            path = replaceAll('/path1/path2/filename.ext','/',pathsep)
            path_res = replaceAll('/path1/path2/filename','/',pathsep)
            _TEST('stripExt, path with extension',stripExt(path) == path_res)
            
            ! Test
            path = replaceAll('/path1/path2/filename','/',pathsep)
            path_res = path
            _TEST('stripExt, path with no extension',stripExt(path) == path_res)
            
            ! Test:
            path = replaceAll('/path1/path2/basename./','/',pathsep)
            path_res = path
            _TEST('stripExt, path with no extension, dot inside the path',stripExt(path) == path)

            ! Test:
            path = replaceAll('/path1/path2/basename.ext/filename','/',pathsep)
            path_res = path
            _TEST('stripExt, path with no extension, dot inside the path (2)',stripExt(path) == path)
      
            
      end function
      

      logical function test_splitExt()
      implicit none
      character(len=max_pathlen)    :: path, path_res, root, root_res, ext, ext_res
      !
            test_splitExt = .false.
            ! Test:
            call splitExt('', root, ext)
            _TEST('splitExt, empty path, string', root == '' .and. ext == '')
            
            ! Test:
            call splitExt('.ext', root, ext)
            _TEST('splitExt, filename that starts with dot', root == '.ext' .and.  ext == '')

            ! Test:
            path = pathsep // '.ext'
            path_res = path
            call splitExt(path, root, ext)
            _TEST('splitExt, filename that starts with dot, root', root == path_res .and.  ext == '')

            ! Test:
            path = 'filename'
            path_res = path
            call splitExt(path, root, ext)
            _TEST('splitExt, file, no extension', root == path_res .and. ext == '')

            ! Test:
            path = 'filename.ext'
            call splitExt(path, root, ext)
            _TEST('splitExt, file with extension',root == 'filename' .and. ext == '.ext')
            
            ! Test:
            path = pathsep // 'filename'
            path_res = path
            call splitExt(path, root, ext)
            _TEST('splitExt, file, no extension, root',root == path_res .and. ext == '')
            
            ! Test:
            path = pathsep // 'filename.ext'
            path_res = pathsep // 'filename'
            call splitExt(path, root, ext)
            _TEST('splitExt, file with extension, root', root == path_res .and. ext == '.ext')
            
            ! Test:
            path = pathsep // 'filename.some.ext'
            path_res = pathsep // 'filename.some'
            call splitExt(path, root, ext)
            _TEST('splitExt, file with dot in name and extension, root', root == path_res .and. ext == '.ext')
            
            ! Test
            path = replaceAll('/path1/path2/filename.ext','/',pathsep)
            path_res = replaceAll('/path1/path2/filename','/',pathsep)
            call splitExt(path, root, ext)
            _TEST('splitExt, path with extension',root == path_res .and. ext == '.ext')
            _TEST('splitExt, path with extension, concatenate', trim(root)//trim(ext) == path)
            
            ! Test
            path = replaceAll('/path1/path2/filename','/',pathsep)
            path_res = path
            call splitExt(path, root, ext)
            _TEST('splitExt, path with no extension', root == path_res .and. ext == '')
            
            ! Test:
            path = replaceAll('/path1/path2/basename./','/',pathsep)
            path_res = path
            call splitExt(path, root, ext)
            _TEST('splitExt, path with no extension, dot inside the path', root == path_res .and. ext == '')

            ! Test:
            path = replaceAll('/path1/path2/basename.ext/filename','/',pathsep)
            path_res = path
            call splitExt(path, root, ext)
            _TEST('splitExt, path with no extension, dot inside the path (2)', root == path_res .and. ext == '')
            
            ! Test:
            path = replaceAll('/path1/path2/basename.ext/filename.ext','/',pathsep)
            path_res = replaceAll('/path1/path2/basename.ext/filename','/',pathsep)
            call splitExt(path, root, ext)
            _TEST('splitExt, path with extension, dot inside the path (2)', root == path_res .and. ext == '.ext')
            
      end function
      
      
      
      
end module      