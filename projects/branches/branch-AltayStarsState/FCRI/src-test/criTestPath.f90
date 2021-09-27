!
! $Id: criTestPath.f90 2700 2016-06-29 20:01:50Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-11-02
!>    $Revision: 2700 $
!>    $Date: 2016-06-29 22:01:50 +0200 (Wed, 29 Jun 2016) $
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTestPath.f90 

#include "criTest.fpp"

module criTestPath
use criPath
use criAlgorithm
use criTest
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

            stat = test_pathjoin()
            
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
      character(len=max_pathlen)    :: path, path_res, root, ext
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
      
      
      logical function test_pathjoin()
      implicit none
      character(len=max_pathlen)    :: path, path_res, path_a, path_b
      !
            test_pathjoin = .false.
            ! Test:
            path_a = ''
            path_b = ''
            path_res = ''
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, empty paths', path == path_res)

            ! Test:
            path_a = '    '
            path_b = '    '
            path_res = ''
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, blank paths', path == path_res)

            ! Test:
            path_a = ''
            path_b = pathsep
            path_res = pathsep
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, empty path + pathsep', path == path_res)
            
            ! Test:
            path_a = '    '
            path_b = pathsep
            path_res = pathsep
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, blank path + pathsep', path == pathsep)

            ! Test:
            path_a = pathsep
            path_b = ''
            path_res = pathsep
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, pathsep + empty path,', path == pathsep)

            ! Test:
            path_a = pathsep
            path_b = '    '
            path_res = pathsep
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, pathsep + blank path,', path == pathsep)

            ! Test:
            path_a = pathsep
            path_b = pathsep
            path_res = pathsep
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, pathsep + pathsep', path == pathsep)

            ! Test:
            path_a = '    '//pathsep
            path_b = ' '//pathsep
            path_res = pathsep
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, blank+pathsep + blank+pathsep', path == pathsep)
            
            ! Test:
            path_a = 'path1'
            path_b = pathsep//'path2'
            path_res = path_b
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, 2nd path starts with root', path == path_res)
            
            ! Test:
            path_a = 'path1'
            path_b = '     '//pathsep//'path2'
            path_res = pathsep//'path2'
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, 2nd path starts with blank-trailed root', path == path_res)
            

            ! Test:
            path_a = 'path1'//pathsep
            path_b = 'path2'
            path_res = 'path1'//pathsep//'path2'
            path = 'path1'//''//'path2'
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, path+pathsep + path', path == path_res)

            ! Test:
            path_a = 'path1'
            path_b = 'path2'
            path_res = 'path1'//pathsep//'path2'
            path = pathjoin(path_a, path_b)
            _TEST('pathjoin, path + path', path == path_res)


            test_pathjoin = .true.
      !      
      end function
      
end module      