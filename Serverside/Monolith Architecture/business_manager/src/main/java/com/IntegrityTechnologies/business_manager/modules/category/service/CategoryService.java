package com.IntegrityTechnologies.business_manager.modules.category.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.exception.CategoryNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.category.mapper.CategoryMapper;
import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.category.repository.CategoryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class CategoryService {

    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;

    // ---------------- SAVE / UPDATE ----------------
    public CategoryDTO saveCategory(CategoryDTO dto) {
        Category category = dto.getId() != null
                ? categoryRepository.findById(dto.getId()).orElse(new Category())
                : new Category();

        categoryRepository.findByNameIgnoreCase(dto.getName())
                .ifPresent(existing -> {
                    if (!existing.getId().equals(dto.getId())) {
                        throw new IllegalArgumentException("Category name already exists: " + dto.getName());
                    }
                });

        categoryMapper.updateEntityFromDTO(dto, category);

        if (dto.getParentId() != null) {
            if (dto.getParentId().equals(dto.getId()))
                throw new IllegalArgumentException("Category cannot be its own parent");

            Category parent = categoryRepository.findById(dto.getParentId())
                    .orElseThrow(() -> new CategoryNotFoundException("Parent category not found"));

            if (isChildOf(parent, category))
                throw new IllegalArgumentException("Cannot set parent: would create cycle");

            category.setParent(parent);
        } else {
            category.setParent(null);
        }

        category = categoryRepository.save(category);

        CategoryDTO result = categoryMapper.toDTO(category);
        result.setSubcategories(
                category.getSubcategories() != null
                        ? category.getSubcategories().stream().map(categoryMapper::toDTO).collect(Collectors.toList())
                        : List.of()
        );
        return result;
    }

    @Transactional
    public CategoryDTO updateCategoryRecursive(CategoryDTO dto) {
        return updateCategoryRecursive(dto, false);
    }

    private CategoryDTO updateCategoryRecursive(CategoryDTO dto, boolean parentDeleted) {
        Category category = categoryRepository.findById(dto.getId())
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));

        // Check for duplicate name
        categoryRepository.findByNameIgnoreCase(dto.getName())
                .ifPresent(existing -> {
                    if (!existing.getId().equals(dto.getId())) {
                        throw new IllegalArgumentException("Category name already exists: " + dto.getName());
                    }
                });

        categoryMapper.updateEntityFromDTO(dto, category);

        // Handle parent
        if (dto.getParentId() != null) {
            if (dto.getParentId().equals(dto.getId()))
                throw new IllegalArgumentException("Category cannot be its own parent");

            Category parent = categoryRepository.findById(dto.getParentId())
                    .orElseThrow(() -> new CategoryNotFoundException("Parent category not found"));

            if (isChildOf(parent, category))
                throw new IllegalArgumentException("Cannot set parent: would create cycle");

            category.setParent(parent);

            // Propagate deleted from parent
            if (parent.isDeleted()) {
                category.setDeleted(true);
                category.setDeletedAt(LocalDateTime.now());
            }
        } else {
            category.setParent(null);
        }

        // If parentDeleted flag is true, override
        if (parentDeleted) {
            category.setDeleted(true);
            category.setDeletedAt(LocalDateTime.now());
        }

        category = categoryRepository.save(category);

        // ------------------ Propagate to all existing children in DB ------------------
        if (category.getSubcategories() != null && !category.getSubcategories().isEmpty()) {
            for (Category child : category.getSubcategories()) {
                updateChildDeletedState(child, category.isDeleted());
            }
        }

        CategoryDTO result = categoryMapper.toDTO(category);
        result.setSubcategories(
                category.getSubcategories() != null
                        ? category.getSubcategories().stream().map(categoryMapper::toDTO).collect(Collectors.toList())
                        : List.of()
        );
        return result;
    }

    /**
     * Recursively propagate deleted/restored state to children from the parent.
     */
    private void updateChildDeletedState(Category child, boolean parentDeleted) {
        boolean changed = false;
        if (parentDeleted && !child.isDeleted()) {
            child.setDeleted(true);
            child.setDeletedAt(LocalDateTime.now());
            changed = true;
        } else if (!parentDeleted && child.isDeleted()) {
            child.setDeleted(false);
            child.setDeletedAt(null);
            changed = true;
        }

        if (changed) {
            categoryRepository.save(child);
        }

        if (child.getSubcategories() != null && !child.getSubcategories().isEmpty()) {
            for (Category sub : child.getSubcategories()) {
                updateChildDeletedState(sub, parentDeleted);
            }
        }
    }

    private boolean isChildOf(Category potentialParent, Category category) {
        if (potentialParent == null) return false;
        if (potentialParent.getId().equals(category.getId())) return true;
        return isChildOf(potentialParent.getParent(), category);
    }

    // ---------------- TREE MODE ----------------
    private CategoryDTO buildCategoryTree(Category category) {
        CategoryDTO dto = categoryMapper.toDTO(category);
        dto.setSubcategories(category.getSubcategories() != null
                ? category.getSubcategories().stream().map(this::buildCategoryTree).collect(Collectors.toList())
                : new ArrayList<>());
        return dto;
    }

    private List<CategoryDTO> buildTree(List<Category> categories) {
        // Step 1: Map all categories to DTOs
        Map<Long, CategoryDTO> dtoMap = categories.stream()
                .map(categoryMapper::toDTO)
                .collect(Collectors.toMap(CategoryDTO::getId, dto -> dto));

        // Step 2: Group children by parent ID
        Map<Long, List<CategoryDTO>> childrenMap = categories.stream()
                .filter(c -> c.getParent() != null)
                .map(categoryMapper::toDTO)
                .collect(Collectors.groupingBy(CategoryDTO::getParentId));

        // Step 3: Attach children recursively
        dtoMap.values().forEach(dto -> {
            List<CategoryDTO> children = childrenMap.getOrDefault(dto.getId(), new ArrayList<>());
            dto.setSubcategories(children);
        });

        // Step 4: Return only root nodes (parent == null OR parent not in deleted list)
        return dtoMap.values().stream()
                .filter(dto -> dto.getParentId() == null || !dtoMap.containsKey(dto.getParentId()))
                .toList();
    }

    public List<CategoryDTO> getAllActiveTree() {
        return buildTree(categoryRepository.findAllActiveWithSubcategories());
    }

    public List<CategoryDTO> getAllDeletedTree() {
        List<Category> allDeleted = categoryRepository.findAllDeleted();
        return buildTree(allDeleted);
    }

    public List<CategoryDTO> getAllIncludingDeletedTree() {
        return buildTree(categoryRepository.findAllIncludingDeletedWithSubcategories());
    }

    // ---------------- FLAT MODE ----------------
    public List<CategoryDTO> getAllActiveFlat() {
        return categoryRepository.findAllActiveFlat().stream().map(categoryMapper::toDTO).collect(Collectors.toList());
    }

    public List<CategoryDTO> getAllDeletedFlat() {
        return categoryRepository.findAllDeletedFlat().stream().map(categoryMapper::toDTO).collect(Collectors.toList());
    }

    public List<CategoryDTO> getAllIncludingDeletedFlat() {
        return categoryRepository.findAllIncludingDeletedFlat().stream().map(categoryMapper::toDTO).collect(Collectors.toList());
    }

    // ---------------- SINGLE CATEGORY ----------------
    public CategoryDTO getCategoryTree(Long id) {
        Category category = categoryRepository.findByIdWithSubcategories(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));
        return buildCategoryTree(category);
    }

    public CategoryDTO getCategoryFlat(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));
        CategoryDTO dto = categoryMapper.toDTO(category);
        dto.setSubcategories(null);
        return dto;
    }

    // Flat active version
    public CategoryDTO getCategoryFlatActive(Long id) {
        Category category = categoryRepository.findByIdAndDeletedFalse(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found or inactive"));
        CategoryDTO dto = categoryMapper.toDTO(category);
        dto.setSubcategories(null); // flat
        return dto;
    }

    // Tree active version
    public CategoryDTO getCategoryTreeActive(Long id) {
        Category category = categoryRepository.findByIdWithSubcategoriesAndActive(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found or inactive"));
        return buildActiveCategoryTree(category);
    }

    // Recursive helper for active categories only
    private CategoryDTO buildActiveCategoryTree(Category category) {
        CategoryDTO dto = categoryMapper.toDTO(category);
        if (category.getSubcategories() != null && !category.getSubcategories().isEmpty()) {
            List<CategoryDTO> subs = category.getSubcategories().stream()
                    .filter(sub -> !sub.isDeleted())
                    .map(this::buildActiveCategoryTree)
                    .toList();
            dto.setSubcategories(subs);
        } else {
            dto.setSubcategories(List.of());
        }
        return dto;
    }


    // ---------------- DELETE / RESTORE ----------------
    public ResponseEntity<ApiResponse> softDelete(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));
        category.setDeleted(true);
        category.setDeletedAt(LocalDateTime.now());
        categoryRepository.save(category);

        if (category.getSubcategories() != null)
            category.getSubcategories().forEach(sub -> softDelete(sub.getId()));

        ApiResponse response = new ApiResponse("success", "Category deleted successfully");
        return ResponseEntity.ok(response);
    }

    public ResponseEntity<?> hardDelete(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));
        categoryRepository.delete(category);

        ApiResponse response = new ApiResponse("success", "Category deleted successfully");
        return ResponseEntity.ok(response);
    }


    // ---------------- RESTORE ----------------
    public ResponseEntity<?> restore(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));
        category.setDeleted(false);
        category.setDeletedAt(null);
        categoryRepository.save(category);

        ApiResponse response = new ApiResponse("success", "Category restored successfully");
        return ResponseEntity.ok(response);
    }

    public ResponseEntity<?> restoreRecursively(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found"));

        restoreCategoryAndChildren(category);
        ApiResponse response = new ApiResponse("success", "Category, and all its sub-categories, restored successfully");
        return ResponseEntity.ok(response);
    }

    /**
     * Helper method to recursively restore a category and all its subcategories.
     */
    private void restoreCategoryAndChildren(Category category) {
        category.setDeleted(false);
        category.setDeletedAt(null);
        categoryRepository.save(category);

        if (category.getSubcategories() != null) {
            category.getSubcategories().forEach(this::restoreCategoryAndChildren);
        }
    }

    // ---------------- SEARCH ----------------

    /**
     * Search and return a hierarchical tree of matching categories.
     * Active only if activeOnly=true, otherwise all categories.
     */
    public List<CategoryDTO> searchByKeyword(String keyword, boolean activeOnly) {
        List<Category> matching = activeOnly
                ? categoryRepository.searchActive(keyword)
                : categoryRepository.searchAll(keyword);

        // Convert to DTOs
        List<CategoryDTO> dtos = matching.stream()
                .map(categoryMapper::toDTO)
                .toList();

        // Build tree
        return buildTreeFromList(dtos);
    }

    /**
     * Build hierarchical tree from a flat list of CategoryDTOs.
     */
    private List<CategoryDTO> buildTreeFromList(List<CategoryDTO> categories) {
        Map<Long, List<CategoryDTO>> childrenMap = categories.stream()
                .filter(c -> c.getParentId() != null)
                .collect(Collectors.groupingBy(CategoryDTO::getParentId));

        List<CategoryDTO> roots = categories.stream()
                .filter(c -> c.getParentId() == null || !categories.stream().anyMatch(p -> p.getId().equals(c.getParentId())))
                .toList();

        for (CategoryDTO root : roots) {
            attachChildren(root, childrenMap);
        }

        return roots;
    }

    private void attachChildren(CategoryDTO parent, Map<Long, List<CategoryDTO>> childrenMap) {
        List<CategoryDTO> children = childrenMap.getOrDefault(parent.getId(), new ArrayList<>());
        parent.setSubcategories(children);
        children.forEach(child -> attachChildren(child, childrenMap));
    }
}