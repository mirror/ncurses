from conans import AutoToolsBuildEnvironment, ConanFile, tools
from conans.errors import ConanInvalidConfiguration
from contextlib import contextmanager
import os


class NCursesConan(ConanFile):
    name = "ncurses"
    settings = "os", "compiler", "build_type", "arch"
    generators = "pkg_config"
    exports_sources = "**"
    options = {
        "shared": [True, False],
        "fPIC": [True, False],
        "with_pcre2": [True, False],
        "with_extended_colors": [True, False],
        "with_progs": [True, False],
        "with_reentrant": [True, False],
        "with_widec": [True, False],
    }
    default_options = {
        "shared": False,
        "fPIC": True,
        "with_progs": True,
        "with_pcre2": False,
        "with_extended_colors": False,
        "with_reentrant": False,
        "with_widec": False,
    }

    _autotools = None

    def set_version(self):
        abi, version, date = tools.load(os.path.join(self.recipe_folder, "VERSION")).strip().split("\t")
        self.version = "{}-{}".format(version, date)

    def config_options(self):
        if self.settings.os == "Windows":
            del self.options.fPIC

    def configure(self):
        if self.options.shared:
            del self.options.fPIC
        if not self.options.with_widec:
            del self.options.with_extended_colors

    def requirements(self):
        if self.options.with_pcre2:
            self.requires("pcre2/10.33")
        if self.settings.compiler == "Visual Studio":
            self.requires("getopt-for-visual-studio/20200201", private=True)
            self.requires("dirent/1.23.2", private=True)
            if self.options.get_safe("with_extended_colors", False):
                self.requires("naive-tsearch/0.1.0")

    def build_requirements(self):
        if tools.os_info.is_windows and not "CONAN_BASH_PATH" in os.environ:
            self.build_requires("msys2/20190524")

    def _configure_autotools(self):
        if self._autotools:
            return self._autotools
        self._autotools = AutoToolsBuildEnvironment(self, win_bash=tools.os_info.is_windows)
        build = None
        host = None
        conf_args = [
            "--with-shared" if self.options.shared else "--without-shared",
            "--with-cxx-shared" if self.options.shared else "--without-cxx-shared",
            "--enable-reentrant" if self.options.with_reentrant else "--disable-reentrant",
            "--with-pcre2" if self.options.with_pcre2 else "--without-pcre2",
            "--enable-widec" if self.options.with_widec else "--disable-widec",
            "--enable-ext-colors" if self.options.get_safe("with_extended_colors", False) else "--disable-ext-colors",
            "--without-libtool",
            "--without-normal" if self.options.shared else "--with-normal",
            "--with-progs" if self.options.with_progs else "--without-progs",
            "--without-ada",
            "--without-manpages",
            "--with-tests",
            "--disable-echo",
            "--with-debug" if self.settings.build_type == "Debug" else "--without-debug",
            "--without-profile",
            "--with-sp-funcs",
            "--disable-rpath",
            "--disable-pc-files",
        ]
        if self.settings.os == "Windows":
            conf_args.extend([
                "--disable-macros",
                "--disable-termcap",
                "--enable-database",
                "--enable-sp-funcs",
                "--enable-term-driver",
                "--enable-interop",
            ])
            self._autotools.flags.append("-D_WIN32_WINNT=0x0600")
        if self.settings.compiler == "Visual Studio":
            conf_args.extend([
                "ac_cv_func_getopt=yes",
            ])
            build = host = "{}-w64-mingw32-msvc".format(self.settings.arch)
            self._autotools.flags.append("-FS")
            self._autotools.cxx_flags.append("-EHsc")
            self._autotools.link_flags.append("-debug")
            self._autotools.libs = []
            if self.options.with_widec:
                self._autotools.defines.append("NAIVE_TSEARCH_HDRONLY")
        self._autotools.configure(args=conf_args, configure_dir=self.source_folder, host=host, build=build)
        return self._autotools

    @contextmanager
    def _build_context(self):
        # FIXME: if cross compiling, set BUILD_CC. Does conan support multiple toolchains?
        if self.settings.compiler == "Visual Studio":
            with tools.vcvars(self.settings):
                msvc_env = {
                    "CC": "cl -nologo",
                    "CXX": "cl -nologo",
                    "LD": "link -nologo",
                    "LDFLAGS": "",
                    "NM": "dumpbin -symbols",
                    "STRIP": ":",
                    "AR": "lib",
                    "RANLIB": ":",
                }
                with tools.environment_append(msvc_env):
                    yield
        else:
            yield

    def build(self):
        with self._build_context():
            autotools = self._configure_autotools()
            autotools.make()

    def package(self):
        with self._build_context():
            autotools = AutoToolsBuildEnvironment(self, win_bash=tools.os_info.is_windows)
            autotools.install()
