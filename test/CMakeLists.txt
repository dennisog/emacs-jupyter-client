########################################################################
# compile the Mock server
########################################################################
set(SRCFILES
  "MockServer.cc"
  )

add_executable(mockserver ${SRCFILES})

link_directories(${CMAKE_BINARY_DIR}/src)
include_directories("../src")

target_link_libraries(mockserver
  emacs-jupyter-client
  ${Boost_LIBRARIES}
  ${JSONCPP_LIBRARY}
  ${ZEROMQ_LIBRARIES}
  )
