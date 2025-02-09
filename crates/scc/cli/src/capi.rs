use minidl::Library;

use crate::raw;

pub(super) fn load() -> anyhow::Result<raw::SccApi> {
    let lib = Library::load("scc_lib.dll")?;
    Ok(unsafe {
        raw::SccApi {
            settings_new: lib.sym("scc_settings_new\0")?,
            settings_set_custom_cache_file: lib.sym("scc_settings_set_custom_cache_file\0")?,
            settings_add_script_path: lib.sym("scc_settings_add_script_path\0")?,
            settings_set_output_cache_file: lib.sym("scc_settings_set_output_cache_file\0")?,
            settings_disable_error_popup: lib.sym("scc_settings_disable_error_popup\0")?,
            compile: lib.sym("scc_compile\0")?,
            free_result: lib.sym("scc_free_result\0")?,
            get_success: lib.sym("scc_get_success\0")?,
            copy_error: lib.sym("scc_copy_error\0")?,
            output_get_source_ref: lib.sym("scc_output_get_source_ref\0")?,
            output_source_ref_count: lib.sym("scc_output_source_ref_count\0")?,
            source_ref_type: lib.sym("scc_source_ref_type\0")?,
            source_ref_is_native: lib.sym("scc_source_ref_is_native\0")?,
            source_ref_name: lib.sym("scc_source_ref_name\0")?,
            source_ref_parent_name: lib.sym("scc_source_ref_parent_name\0")?,
            source_ref_path: lib.sym("scc_source_ref_path\0")?,
            source_ref_line: lib.sym("scc_source_ref_line\0")?,
        }
    })
}

#[cfg(test)]
mod tests {
    use std::mem;

    use raw::*;

    use super::*;

    #[test]
    fn api_functions_are_stable() {
        // this test just ensures that the API types haven't changed
        // existing function signatures are not allowed to change due to backwards compatibility

        let SccApi {
            settings_new,
            settings_set_custom_cache_file,
            settings_set_output_cache_file,
            settings_add_script_path,
            settings_disable_error_popup,
            compile,
            free_result,
            get_success,
            copy_error,
            output_get_source_ref,
            output_source_ref_count,
            source_ref_type,
            source_ref_is_native,
            source_ref_name,
            source_ref_parent_name,
            source_ref_path,
            source_ref_line,
        } = load().unwrap();

        let _settings_new: unsafe extern "C" fn(*const i8) -> *mut SccSettings =
            settings_new.unwrap();
        let _settings_set_custom_cache_file: unsafe extern "C" fn(*mut SccSettings, *const i8) =
            settings_set_custom_cache_file.unwrap();
        let _settings_set_output_cache_file: unsafe extern "C" fn(*mut SccSettings, *const i8) =
            settings_set_output_cache_file.unwrap();
        let _settings_add_script_path: unsafe extern "C" fn(*mut SccSettings, *const i8) =
            settings_add_script_path.unwrap();
        let _settings_disable_error_popup: unsafe extern "C" fn(*mut SccSettings) =
            settings_disable_error_popup.unwrap();
        let _compile: unsafe extern "C" fn(*mut SccSettings) -> *mut SccResult = compile.unwrap();
        let _free_result: unsafe extern "C" fn(*mut SccResult) = free_result.unwrap();
        let _get_success: unsafe extern "C" fn(*mut SccResult) -> *mut SccOutput =
            get_success.unwrap();
        let _copy_error: unsafe extern "C" fn(*mut SccResult, *mut i8, usize) -> usize =
            copy_error.unwrap();
        let _output_get_source_ref: unsafe extern "C" fn(
            *mut SccOutput,
            usize,
        ) -> *mut SccSourceRef = output_get_source_ref.unwrap();
        let _output_source_ref_count: unsafe extern "C" fn(*mut SccOutput) -> usize =
            output_source_ref_count.unwrap();
        let _source_ref_type: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> u8 =
            source_ref_type.unwrap();
        let _source_ref_is_native: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> bool =
            source_ref_is_native.unwrap();
        let _source_ref_name: unsafe extern "C" fn(
            *mut SccOutput,
            *mut SccSourceRef,
        ) -> StrWithLen = source_ref_name.unwrap();
        let _source_ref_parent_name: unsafe extern "C" fn(
            *mut SccOutput,
            *mut SccSourceRef,
        ) -> StrWithLen = source_ref_parent_name.unwrap();
        let _source_ref_path: unsafe extern "C" fn(
            *mut SccOutput,
            *mut SccSourceRef,
        ) -> StrWithLen = source_ref_path.unwrap();
        let _source_ref_line: unsafe extern "C" fn(*mut SccOutput, *mut SccSourceRef) -> usize =
            source_ref_line.unwrap();
    }

    #[test]
    fn api_types_are_stable() {
        assert_eq!(mem::size_of::<StrWithLen>(), 16);
        assert_eq!(mem::offset_of!(StrWithLen, str_), 0);
        assert_eq!(mem::offset_of!(StrWithLen, len), 8);

        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_UNDEFINED, 0);
        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_CLASS, 1);
        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_FIELD, 2);
        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_FUNCTION, 3);
        assert_eq!(SccSourceRefType_SCC_SOURCE_REF_TYPE_ENUM, 4);
    }
}
