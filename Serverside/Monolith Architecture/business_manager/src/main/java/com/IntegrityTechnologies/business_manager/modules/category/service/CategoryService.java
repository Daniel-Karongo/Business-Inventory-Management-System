package com.IntegrityTechnologies.business_manager.modules.category.service;

import com.IntegrityTechnologies.business_manager.exception.CategoryNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.category.dto.CategoryDTO;
import com.IntegrityTechnologies.business_manager.modules.category.mapper.CategoryMapper;
import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.category.repository.CategoryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CategoryService {

    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;

    public CategoryDTO saveCategory(CategoryDTO dto) {
        Category category = dto.getId() != null
                ? categoryRepository.findById(dto.getId()).orElse(new Category())
                : new Category();

        categoryMapper.updateEntityFromDTO(dto, category);
        category = categoryRepository.save(category);

        return categoryMapper.toDTO(category);
    }

    public List<CategoryDTO> getAllIncludingDeleted() {
        return categoryRepository.findAllIncludingDeleted().stream()
                .map(categoryMapper::toDTO)
                .collect(Collectors.toList());
    }

    public List<CategoryDTO> getAllActiveCategories() {
        return categoryRepository.findAllActiveCategories().stream()
                .map(categoryMapper::toDTO)
                .collect(Collectors.toList());
    }

    public List<CategoryDTO> getDeletedCategories() {
        return categoryRepository.findDeletedCategories().stream()
                .map(categoryMapper::toDTO)
                .collect(Collectors.toList());
    }

    public CategoryDTO getCategory(Long id) {
        return categoryRepository.findById(id)
                .map(categoryMapper::toDTO)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found with ID: " + id));
    }

    public void restoreCategory(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found with ID: " + id));
        category.setDeleted(false);
        categoryRepository.save(category);
    }

    public void softDeleteCategory(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found with ID: " + id));
        category.setDeleted(true);
        categoryRepository.save(category);
    }

    public void hardDeleteCategory(Long id) {
        Category category = categoryRepository.findById(id)
                .orElseThrow(() -> new CategoryNotFoundException("Category not found with ID: " + id));
        categoryRepository.delete(category);
    }
}