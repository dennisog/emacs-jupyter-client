########################################################################
# Setup the compilation of the shared library
########################################################################
set(SRCFILES
  "EmacsInterface.cc"
  "JupyterClient.cc"
  "BBSocket.cc"
  "Message.cc"
  "Crypto.cc"
  "Handlers.cc"
  )

add_library(emacs-jupyter-client SHARED ${SRCFILES})
if(WIN32)
  install(TARGETS emacs-jupyter-client RUNTIME DESTINATION src)
else()
  install(TARGETS emacs-jupyter-client LIBRARY DESTINATION src)
endif()

target_link_libraries(emacs-jupyter-client
  ${Boost_LIBRARIES}
  ${JSONCPP_LIBRARY}
  ${ZEROMQ_LIBRARIES}
  ${GCRYPT_LIBRARIES}
  )
