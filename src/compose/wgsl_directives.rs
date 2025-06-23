use std::collections::HashSet;

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
}
