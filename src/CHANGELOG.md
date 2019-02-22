# Changelog for B9

## 0.5.62

* Rewrite `B9.Shake.SharedImageRules` in line with 
  Shake's example for custom rules  

* Replace `ConcatableSyntax` by using `Binary` instances

* Add a newtype wrapper around `YamlObject` for **cloud-init** yaml documents
  `CloudConfigYaml`
  
  This new type serves the purpose of add the `#cloud-config`
  line to the top of the generated yaml document, 
  as [required by cloud-init](https://cloudinit.readthedocs.io/en/latest/topics/format.html#cloud-config-data).
  
  The `Binary` instance adds this **header line** and 
  delegates the rendering of the yaml document to 
  `YamlObject`.
  
* Remove the rendering of this header line in the `YamlObject` 
  `Binary` instance.

* Rename `RenderYaml` to `RenderYamlObject` In order to prevent unexpected 
  runtime behaviour in code that uses this library.