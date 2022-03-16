# $Id: utilities.cmake 2069 2015-02-20 17:22:41Z jgawad $

# Collection of useful utilities

# Converts a CMake list to a space-delimited string
function(list2string LIST_NAME OUTPUT_VAR)
  # See CMake FAQ: http://www.cmake.org/Wiki/CMake_FAQ#How_to_convert_a_semicolon_separated_list_to_a_whitespace_separated_string.3F
  set(NEW_STRING)
  foreach(ITEM ${${LIST_NAME}})
    set(NEW_STRING "${NEW_STRING} ${ITEM}")
  endforeach()
  string(STRIP "${NEW_STRING}" NEW_STRING)
  set(${OUTPUT_VAR} "${NEW_STRING}" PARENT_SCOPE)
endfunction()


