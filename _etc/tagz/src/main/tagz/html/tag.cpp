#include "tag.hpp"

namespace tagz {
namespace html {

tag::tag(const std::string &name, std::initializer_list<std::string> content) :
    tagz::base::tag("<" + name + ">", "</" + name + ">", content)
{}

a::a(std::initializer_list<std::string> content) : tag("a", content)
{}

div::div(std::initializer_list<std::string> content) : tag("div", content)
{}

} // namespace html
} // namespace tagz
