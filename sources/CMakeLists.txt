target_sources(cargo
	PRIVATE
    include/cargo/cargo.h
  )

target_include_directories(cargo INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)
