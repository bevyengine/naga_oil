use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnableDirective {
    pub extensions: Vec<String>,
    pub source_location: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RequiresDirective {
    pub extensions: Vec<String>,
    pub source_location: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DiagnosticDirective {
    pub severity: String,
    pub rule: String,
    pub source_location: usize,
}

#[derive(Debug, Clone, Default)]
pub struct WgslDirectives {
    pub enables: Vec<EnableDirective>,
    pub requires: Vec<RequiresDirective>,
    pub diagnostics: Vec<DiagnosticDirective>,
}

impl WgslDirectives {
    pub fn merge_with(&mut self, other: &WgslDirectives) -> Result<(), super::ComposerErrorInner> {
        for enable in &other.enables {
            for ext in &enable.extensions {
                if !self.has_enable_extension(ext) {
                    self.enables.push(enable.clone());
                    break;
                }
            }
        }

        for requires in &other.requires {
            for ext in &requires.extensions {
                if !self.has_requires_extension(ext) {
                    self.requires.push(requires.clone());
                    break;
                }
            }
        }

        for diagnostic in &other.diagnostics {
            if let Some(existing) = self.diagnostics.iter().find(|d| d.rule == diagnostic.rule) {
                if existing.severity != diagnostic.severity {
                    return Err(super::ComposerErrorInner::ConflictingDirectives {
                        rule: diagnostic.rule.clone(),
                        existing_severity: existing.severity.clone(),
                        new_severity: diagnostic.severity.clone(),
                    });
                }
            } else {
                self.diagnostics.push(diagnostic.clone());
            }
        }

        Ok(())
    }

    fn has_enable_extension(&self, extension: &str) -> bool {
        self.enables
            .iter()
            .any(|enable| enable.extensions.contains(&extension.to_string()))
    }

    fn has_requires_extension(&self, extension: &str) -> bool {
        self.requires
            .iter()
            .any(|requires| requires.extensions.contains(&extension.to_string()))
    }

    pub fn to_wgsl_string(&self) -> String {
        let mut result = String::new();

        let mut all_enables = HashSet::new();
        for enable in &self.enables {
            all_enables.extend(enable.extensions.iter().cloned());
        }
        if !all_enables.is_empty() {
            let mut enables: Vec<_> = all_enables.into_iter().collect();
            enables.sort();
            result.push_str(&format!("enable {};\n", enables.join(", ")));
        }

        let mut all_requires = HashSet::new();
        for requires in &self.requires {
            all_requires.extend(requires.extensions.iter().cloned());
        }
        if !all_requires.is_empty() {
            let mut requires: Vec<_> = all_requires.into_iter().collect();
            requires.sort();
            result.push_str(&format!("requires {};\n", requires.join(", ")));
        }

        for diagnostic in &self.diagnostics {
            result.push_str(&format!(
                "diagnostic({}, {});\n",
                diagnostic.severity, diagnostic.rule
            ));
        }

        if !result.is_empty() {
            result.push('\n'); // Add blank line after directives
        }

        result
    }

    pub fn is_empty(&self) -> bool {
        self.enables.is_empty() && self.requires.is_empty() && self.diagnostics.is_empty()
    }

    pub fn enable_extensions(&self) -> HashSet<String> {
        let mut extensions = HashSet::new();
        for enable in &self.enables {
            extensions.extend(enable.extensions.iter().cloned());
        }
        extensions
    }

    pub fn requires_extensions(&self) -> HashSet<String> {
        let mut extensions = HashSet::new();
        for requires in &self.requires {
            extensions.extend(requires.extensions.iter().cloned());
        }
        extensions
    }

    pub fn diagnostic_rules(&self) -> HashMap<String, String> {
        let mut rules = HashMap::new();
        for diagnostic in &self.diagnostics {
            rules.insert(diagnostic.rule.clone(), diagnostic.severity.clone());
        }
        rules
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wgsl_directives_empty() {
        let directives = WgslDirectives::default();
        assert!(directives.is_empty());
        assert_eq!(directives.to_wgsl_string(), "");
    }

    #[test]
    fn test_wgsl_directives_to_string() {
        let mut directives = WgslDirectives::default();
        directives.enables.push(EnableDirective {
            extensions: vec!["f16".to_string(), "subgroups".to_string()],
            source_location: 0,
        });
        directives.requires.push(RequiresDirective {
            extensions: vec!["readonly_and_readwrite_storage_textures".to_string()],
            source_location: 0,
        });
        directives.diagnostics.push(DiagnosticDirective {
            severity: "warn".to_string(),
            rule: "derivative_uniformity".to_string(),
            source_location: 0,
        });

        let result = directives.to_wgsl_string();
        assert!(result.contains("enable f16, subgroups;"));
        assert!(result.contains("requires readonly_and_readwrite_storage_textures;"));
        assert!(result.contains("diagnostic(warn, derivative_uniformity);"));
    }

    #[test]
    fn test_merge_directives_no_conflict() {
        let mut base = WgslDirectives::default();
        base.enables.push(EnableDirective {
            extensions: vec!["f16".to_string()],
            source_location: 0,
        });

        let other = WgslDirectives {
            enables: vec![EnableDirective {
                extensions: vec!["subgroups".to_string()],
                source_location: 0,
            }],
            requires: vec![RequiresDirective {
                extensions: vec!["readonly_and_readwrite_storage_textures".to_string()],
                source_location: 0,
            }],
            diagnostics: vec![DiagnosticDirective {
                severity: "warn".to_string(),
                rule: "derivative_uniformity".to_string(),
                source_location: 0,
            }],
        };

        base.merge_with(&other).unwrap();

        let enables = base.enable_extensions();
        assert!(enables.contains("f16"));
        assert!(enables.contains("subgroups"));

        let requires = base.requires_extensions();
        assert!(requires.contains("readonly_and_readwrite_storage_textures"));

        let diagnostics = base.diagnostic_rules();
        assert_eq!(
            diagnostics.get("derivative_uniformity"),
            Some(&"warn".to_string())
        );
    }

    #[test]
    fn test_merge_directives_conflict() {
        let mut base = WgslDirectives::default();
        base.diagnostics.push(DiagnosticDirective {
            severity: "warn".to_string(),
            rule: "derivative_uniformity".to_string(),
            source_location: 0,
        });

        let other = WgslDirectives {
            enables: vec![],
            requires: vec![],
            diagnostics: vec![DiagnosticDirective {
                severity: "error".to_string(),
                rule: "derivative_uniformity".to_string(),
                source_location: 0,
            }],
        };

        let result = base.merge_with(&other);
        assert!(result.is_err());
    }
}
