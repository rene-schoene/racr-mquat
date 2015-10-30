## Kinds of changes

### Update

- init
- two times: change of value in first resource
- change of values in first three resources
- change of values in all resource to low value
- three times: change of values in all resource to random value

### SW

- init
- three times: create a new mode for each implementation of each component
- three times: remove half all available modes

### Res

- init
- remove first resource from tree (1)
- add previously (1) removed resource to tree again
- remove half of the resources from tree (2)
- add previously (2) removed resources to tree again
- remove half of the resources from tree and change values of remaining resources (3)
- add previously (3) removed resources to tree again

### Complex

- init (4 sw component, N containers)
- change values of containers
- create new modes for each implementation
- change values of containers
- add new component
- change values of containers
- add new containers
- change values of containers
- (change nothing)
- remove some containers
- remove some modes

## Strategies

### Normal/Incremental

- normal RACR execution, cached used and untouched between steps

## Flushed

- normal RACR execution, cached used
- cache is flushed after generation of the ILP for each change

## Noncached

- all ILP-generation related attributes are note cached
- basic attributes (e.g. compute list of valid hardware containers)
