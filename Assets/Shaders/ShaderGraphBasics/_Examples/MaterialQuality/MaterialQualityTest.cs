using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.UI;

public class MaterialQualityTest : MonoBehaviour
{
    [SerializeField] private Dropdown dropQuality;

    void Start()
    {
        dropQuality.onValueChanged.AddListener((value)=> MaterialQualityUtilities.SetGlobalShaderKeywords((MaterialQuality)value));
    }
}
