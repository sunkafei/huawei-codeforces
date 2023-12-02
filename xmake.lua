add_rules("mode.debug", "mode.release")

target("huawei-codeforces")
    set_kind("binary")
    add_files("src/*.cpp")
    set_languages("c++20")
    add_defines("__SMZ_NATIVE")
    if is_mode("debug") then 
        add_cflags("-fomit-frame-pointer")
        add_cxflags("-fsanitize=address", "-ftrapv")
        add_mxflags("-fsanitize=address", "-ftrapv")
        add_ldflags("-fsanitize=address")
        --set_optimize("fastest")
    end
-- fast read