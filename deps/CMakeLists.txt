include(FetchContent)

# Setup directory to which dependencies will be downloaded
cmake_path(APPEND FETCHCONTENT_BASE_DIR "${CMAKE_SOURCE_DIR}/../" "deps_content" "${CMAKE_GENERATOR_NAME_WITHOUT_SPACES}")

# If dependencies are not available then download it and make available
FetchContent_Declare(
	user_literals
	GIT_REPOSITORY https://github.com/rokoDev/user_literals.git
	GIT_TAG        5c62773ff93b21cc5868eb3c7377deb9a8d8ec97
)

FetchContent_Declare(
	strong_type
	GIT_REPOSITORY https://github.com/rokoDev/strong_type.git
	GIT_TAG        b34f11ad2cbb164030a56a80e488cfd716d43b5d
)

FetchContent_Declare(
	utils
	GIT_REPOSITORY https://github.com/rokoDev/utils.git
	GIT_TAG        f757160958dc89f0c0071d6957043e470a985bec
)

FetchContent_Declare(
	type_name
	GIT_REPOSITORY https://github.com/rokoDev/type_name.git
	GIT_TAG        05ce55919dc1dc55a3d87100f676d21c28fa42f7
)

# If necessary it's possible to use utils from different location instead of downloaded from GitHub
# string(TOUPPER utils UP_utils)
# set(FETCHCONTENT_SOURCE_DIR_${UP_utils} ${CMAKE_SOURCE_DIR}/../utils)

FetchContent_MakeAvailable(user_literals strong_type utils type_name)
target_link_libraries(cargo INTERFACE user_literals strong_type utils type_name)

# If use IDE add targets into deps source group
set_target_properties(user_literals strong_type utils type_name PROPERTIES FOLDER deps)
