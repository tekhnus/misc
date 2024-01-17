#include "tag.hpp"

namespace tagz {
namespace xterm {

tag::tag(const std::string &style, std::initializer_list<std::string> content) :
    tagz::base::tag(style, "\x1b[0m", content)
{}

red::red(std::initializer_list<std::string> content) :
    tag("\x1b[33m", content)
{}

ul::ul(std::initializer_list<std::string> content) :
    tag("\x1b[4m", content)
{}

} // namespace xterm
} // namespace tagz
