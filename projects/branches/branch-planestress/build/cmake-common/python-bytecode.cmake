cmake_minimum_required(VERSION 3.3)

add_custom_target(python-bytecode ALL 
                  COMMAND python -OO -m compileall -q python
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
		  COMMENT "Building Python bytecode in ${CMAKE_CURRENT_SOURCE_DIR}/python")

