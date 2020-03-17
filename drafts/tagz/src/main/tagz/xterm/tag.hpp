#ifndef TAGZ_XTERM_TAG_HPP
#define TAGZ_XTERM_TAG_HPP

#include "../base/tag.hpp"

namespace tagz {
namespace xterm {

class tag : public tagz::base::tag
{
public:
    tag(const std::string &style, std::initializer_list<std::string> content);
};

class red : public tag
{
public:
    red(std::initializer_list<std::string> content);
};

class ul : public tag
{
public:
    ul(std::initializer_list<std::string> content);
};

} // namespace xterm
} // namespace tagz

#endif // TAGZ_XTERM_TAG_HPP
