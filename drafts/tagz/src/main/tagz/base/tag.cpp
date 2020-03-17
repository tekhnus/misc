#include "tag.hpp"

namespace tagz {
namespace base {

tag::tag(const std::string &prefix, const std::string &suffix, std::initializer_list<std::string> content)
{
    value.append(prefix);
    for (auto item : content)
        value.append(item);
    value.append(suffix);
}

tagz::base::tag::operator const std::string&()
{
    return value;
}

} // namespace base
} // namespace tagz
