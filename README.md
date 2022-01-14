<div align="center">

# filtag
Small file-tagging utility for easy searching and filtering in the future.

</div>

## Installation
Install to `.cabal/bin` with
```
cabal install
```

## Usage
Lists available tags
```
filtag list [verbose]
```

Lists files associated with {tag}
```
filtag find {tag}
```

Add {tags} to {file}
```
filtag tag {file} {tags}
```

Add {tags} to {file}
```
filtag remove {file} {tags}
```

Any of the above commands
generates a `.tags` file in the directory
containing the serialized tag mapping.

