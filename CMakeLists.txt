########################################################################
# Project setup
########################################################################
cmake_minimum_required(VERSION 3.3)

project(emacs-jupyter-client)

# prevent in-tree build.
if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message(FATAL_ERROR "Prevented in-tree build. This is bad practice.")
endif(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})


# export the compiler commands (-I,-Wall, etc.) for YouCompleteMe & rtags
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

# we are modern gentlemen and ladies
set(CMAKE_CXX_STANDARD 14)

# compiler flags
# enable warnings
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR
    "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set(warnings "-Wall -Wextra -Werror")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    set(warnings "/W4 /WX /EHsc")
endif()

# we want position independent code
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

########################################################################
# Find libraries
########################################################################
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
find_package(Boost "1.58" REQUIRED COMPONENTS system)
find_package(JsonCpp REQUIRED)
find_package(ZeroMQ 4.0 REQUIRED)
find_package(GCrypt REQUIRED)

# We need the emacs module headers
set(EMACS_SRC_DIR "" CACHE STRING "Directory containing Emacs sources")
message(STATUS "Using Emacs source directory " ${EMACS_SRC_DIR})

########################################################################
# Include paths
########################################################################
include_directories(
  ${EMACS_SRC_DIR}/src
  ${ZEROMQ_INCLUDE_DIRS}
  ${JSONCPP_INCLUDE_DIRS}
  ${GCRYPT_INCLUDE_DIRS}
  )

########################################################################
# Subdirectories
########################################################################
add_subdirectory(src)
add_subdirectory(lisp)
add_subdirectory(test)
