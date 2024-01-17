from context import tests_dir
from overdata import (
    compose,
    product,
    ConcatenateJSON,
    ExtractColumn,
    Add,
    Sum,
    PartitionBy
)


plan = compose(
    ConcatenateJSON(tests_dir / "example.json"),
    PartitionBy(ExtractColumn("key")),
    Sum(ExtractColumn("value"))
)
for row in plan.worker().close():
    print(row)
