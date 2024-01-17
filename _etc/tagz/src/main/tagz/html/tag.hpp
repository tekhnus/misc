#ifndef TAGZ_HTML_TAG_HPP
#define TAGZ_HTML_TAG_HPP

#include "../base/tag.hpp"

namespace tagz {
namespace html {

class tag : public tagz::base::tag
{
public:
    tag(const std::string &name, std::initializer_list<std::string> content);
};

class a : public tag
{
public:
    a(std::initializer_list<std::string> content);
};

tag a_href(const std::string &href);

class div : public tag
{
public:
    div(std::initializer_list<std::string> content);
};

} // namespace html
} // namespace tagz

#endif // TAGZ_HTML_TAG_HPP
